module App.DrillApp (component) where

import Prelude

import Data.Array ((:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (contains)
import Data.String.CodeUnits (takeWhile)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Steno.ChordView (chordSvg)
import Steno.Outline (parseStroke)
import Steno.Storage (loadList, saveList)
import Steno.WordBank (WordEntry, parseWordList)
import Web.HTML.HTMLElement (focus)

type State =
  { wordBank :: Maybe (NonEmptyArray WordEntry)
  , listText :: String
  , loadError :: Maybe String
  , index :: Int
  , typed :: String
  , locked :: Boolean
  , showHint :: Boolean
  }

data Action = Init | UpdateListText String | LoadWordList | EditWordList | HandleInput String | Resolve Boolean | NextWord | AutoHint Int | RefocusInput

initialState :: State
initialState = { wordBank: Nothing, listText: "", loadError: Nothing, index: 0, typed: "", locked: false, showHint: false }

inputRef :: H.RefLabel
inputRef = H.RefLabel "stroke-capture"

-- | Plover (with the machine's dictionary output disabled) types each raw,
-- | untranslated stroke followed by a space, exactly as it would after a
-- | normal translated word. Everything up to that space is one stroke.
strokeDelimiter :: String
strokeDelimiter = " "

-- | How long to wait, with no correct stroke on the current word, before
-- | outlining the answer automatically. Keeps the learner's hands on the
-- | steno keyboard instead of having to reach for a "show hint" button.
hintDelay :: Milliseconds
hintDelay = Milliseconds 5000.0

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    saved <- H.liftEffect loadList
    for_ saved tryLoad
  UpdateListText v -> H.modify_ _ { listText = v }
  LoadWordList -> do
    state <- H.get
    tryLoad state.listText
  EditWordList -> do
    listText <- H.gets _.listText
    H.put initialState { listText = listText }
  HandleInput v -> do
    state <- H.get
    if state.locked then
      pure unit
    else if contains (Pattern strokeDelimiter) v then do
      let stroke = takeWhile (_ /= ' ') v
      entry <- H.gets currentEntry
      let correct = maybe false (\e -> parseStroke stroke == parseStroke e.stroke) entry
      H.modify_ _ { typed = stroke, locked = true }
      void $ H.fork do
        H.liftAff (Aff.delay (Milliseconds 700.0))
        handleAction (Resolve correct)
    else
      H.modify_ _ { typed = v }
  Resolve correct -> do
    H.modify_ _ { typed = "", locked = false }
    if correct then handleAction NextWord else focusInput
  NextWord -> do
    H.modify_ \s -> s { index = s.index + 1, showHint = false }
    scheduleHint
    focusInput
  AutoHint forIndex -> do
    state <- H.get
    when (state.index == forIndex) $ H.modify_ _ { showHint = true }
  RefocusInput -> focusInput

-- | Parses and validates text as a word list. On success, saves it (so
-- | it survives a reload) and starts the drill from the top; on failure,
-- | leaves the textbox in place with an error instead of losing the
-- | learner's input.
tryLoad :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
tryLoad text = case parseWordList text of
  Left err -> H.modify_ _ { wordBank = Nothing, listText = text, loadError = Just err }
  Right wb -> do
    H.liftEffect (saveList text)
    H.put initialState { wordBank = Just wb, listText = text }
    scheduleHint
    focusInput

focusInput :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
focusInput = do
  el <- H.getHTMLElementRef inputRef
  for_ el (H.liftEffect <<< focus)

-- | Schedules the hint reveal for whichever word is current right now. The
-- | captured index guards against showing a stale hint if the word has
-- | already advanced by the time this fires.
scheduleHint :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
scheduleHint = do
  forIndex <- H.gets _.index
  void $ H.fork do
    H.liftAff (Aff.delay hintDelay)
    handleAction (AutoHint forIndex)

currentEntry :: State -> Maybe WordEntry
currentEntry state = entryAt state.index <$> state.wordBank

entryAt :: Int -> NonEmptyArray WordEntry -> WordEntry
entryAt index wb = fromMaybe (NEA.head wb) (NEA.index wb (index `mod` NEA.length wb))

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "drill-app")
    , HE.onClick (\_ -> RefocusInput)
    ]
    (HH.h1_ [ HH.text "Lapwing Steno Drill" ] : body)
  where
  body = case state.wordBank of
    Nothing -> renderLoader state
    Just wb -> renderDrill wb state

renderLoader :: forall m. State -> Array (H.ComponentHTML Action () m)
renderLoader state =
  [ HH.p_
      [ HH.text "Paste a word list below - one \"word<TAB>STROKE\" pair per line, e.g. lines from a lapwing-for-beginners practice file." ]
  , HH.textarea
      [ HP.class_ (HH.ClassName "list-input")
      , HP.value state.listText
      , HP.rows 12
      , HE.onValueInput UpdateListText
      ]
  ]
    <> loadErrorView
    <>
      [ HH.div
          [ HP.class_ (HH.ClassName "drill-actions") ]
          [ HH.button
              [ HP.class_ (HH.ClassName "next-word-btn")
              , HE.onClick (\_ -> LoadWordList)
              ]
              [ HH.text "Load word list" ]
          ]
      ]
  where
  loadErrorView = case state.loadError of
    Nothing -> []
    Just err -> [ HH.pre [ HP.class_ (HH.ClassName "list-error") ] [ HH.text err ] ]

renderDrill :: forall m. NonEmptyArray WordEntry -> State -> Array (H.ComponentHTML Action () m)
renderDrill wb state =
  [ HH.div
      [ HP.class_ (HH.ClassName "drill-columns") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "word-panel") ]
          [ HH.text entry.word ]
      , HH.div
          [ HP.class_ (HH.ClassName "chord-view") ]
          [ chordSvg
              { pressed: parseStroke state.typed
              , expected: parseStroke entry.stroke
              , hint: state.showHint
              }
          ]
      ]
  , HH.input
      [ HP.ref inputRef
      , HP.class_ (HH.ClassName "stroke-capture")
      , HP.value state.typed
      , HP.autofocus true
      , HE.onValueInput HandleInput
      ]
  , HH.div
      [ HP.class_ (HH.ClassName "drill-actions") ]
      [ HH.button
          [ HP.class_ (HH.ClassName "next-word-btn")
          , HE.onClick (\_ -> NextWord)
          ]
          [ HH.text "Next word" ]
      , HH.button
          [ HP.class_ (HH.ClassName "next-word-btn")
          , HE.onClick (\_ -> EditWordList)
          ]
          [ HH.text "Change word list" ]
      ]
  ]
  where
  entry = entryAt state.index wb

