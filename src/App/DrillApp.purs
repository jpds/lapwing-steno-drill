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
import Data.Traversable (traverse)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Steno.ChordView (chordSvg)
import Steno.Outline (parseStroke)
import Steno.Shuffle (shuffle)
import Steno.Storage (loadList, saveList)
import Steno.WordBank (WordEntry, parseWordList)
import Web.HTML.HTMLElement (focus)

type State =
  { wordBank :: Maybe (NonEmptyArray WordEntry)
  , listText :: String
  , loadError :: Maybe String
  , index :: Int
  , typed :: String
  , showHint :: Boolean
  , completed :: Boolean
  , lastStroke :: Maybe { stroke :: String, correct :: Boolean }
  }

data Action
  = Init
  | UpdateListText String
  | LoadWordList
  | EditWordList
  | HandleInput String
  | NextWord
  | AutoHint Int
  | RefocusInput
  | Restart

initialState :: State
initialState =
  { wordBank: Nothing
  , listText: ""
  , loadError: Nothing
  , index: 0
  , typed: ""
  , showHint: false
  , completed: false
  , lastStroke: Nothing
  }

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

-- | How long a resolved stroke's colors stay on screen before a correct
-- | one advances to the next word. Short enough not to bottleneck a fast
-- | typist (steno speeds rarely exceed a stroke every ~200ms), long enough
-- | to register as a flash rather than an instant cut.
flashDelay :: Milliseconds
flashDelay = Milliseconds 150.0

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

-- | Input is never blocked: as soon as a stroke resolves (right or wrong),
-- | `typed` clears immediately so the field is ready for the next raw
-- | stroke with no risk of it concatenating onto a stale value. The
-- | resolved stroke's colors keep showing via `lastStroke` (see
-- | `renderDrill`) until either a new keystroke overwrites it, or, for a
-- | correct stroke only, `flashDelay` elapses and `NextWord` moves on.
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
    focusInput
  HandleInput v -> do
    state <- H.get
    if state.completed then
      pure unit
    else if contains (Pattern strokeDelimiter) v then do
      let stroke = takeWhile (_ /= ' ') v
      entry <- H.gets currentEntry
      let correct = maybe false (\e -> parseStroke stroke == parseStroke e.stroke) entry
      H.modify_ _ { typed = "", lastStroke = Just { stroke, correct } }
      if correct then
        void $ H.fork do
          H.liftAff (Aff.delay flashDelay)
          handleAction NextWord
      else
        focusInput
    else
      H.modify_ _ { typed = v, lastStroke = Nothing }
  NextWord -> do
    state <- H.get
    case state.wordBank of
      Nothing -> pure unit
      Just wb -> do
        let newIndex = state.index + 1
        H.modify_ _ { index = newIndex, showHint = false, lastStroke = Nothing }
        if newIndex >= NEA.length wb then
          H.modify_ _ { completed = true }
        else do
          scheduleHint
          focusInput
  AutoHint forIndex -> do
    state <- H.get
    when (state.index == forIndex && not state.completed) $ H.modify_ _ { showHint = true }
  RefocusInput -> focusInput
  Restart -> do
    state <- H.get
    reshuffled <- H.liftEffect (traverse shuffle state.wordBank)
    H.put initialState { wordBank = reshuffled, listText = state.listText }
    scheduleHint
    focusInput

-- | Parses and validates text as a word list. On success, saves it (so
-- | it survives a reload) and starts the drill from the top; on failure,
-- | leaves the textbox in place with an error instead of losing the
-- | learner's input.
tryLoad :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
tryLoad text = case parseWordList text of
  Left err -> H.modify_ _ { wordBank = Nothing, listText = text, loadError = Just err }
  Right wb -> do
    H.liftEffect (saveList text)
    shuffled <- H.liftEffect (shuffle wb)
    H.put initialState { wordBank = Just shuffled, listText = text }
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

-- | The word bank always loops via mod, and NextWord marks the drill
-- | completed the instant index would run past the end, so this
-- | fromMaybe fallback is unreachable in practice; it just keeps
-- | NEA.index total without threading a partiality proof through.
entryAt :: NonEmptyArray WordEntry -> Int -> WordEntry
entryAt wb index = fromMaybe (NEA.head wb) (NEA.index wb (index `mod` NEA.length wb))

currentEntry :: State -> Maybe WordEntry
currentEntry state = (\wb -> entryAt wb state.index) <$> state.wordBank

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
    Just wb
      | state.completed -> renderCompleted wb
      | otherwise -> renderDrill wb state

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

renderCompleted :: forall m. NonEmptyArray WordEntry -> Array (H.ComponentHTML Action () m)
renderCompleted wb =
  [ HH.div
      [ HP.class_ (HH.ClassName "drill-complete") ]
      [ HH.text ("Drill complete! You went through all " <> show (NEA.length wb) <> " words.") ]
  , HH.div
      [ HP.class_ (HH.ClassName "drill-actions") ]
      [ HH.button
          [ HP.class_ (HH.ClassName "next-word-btn")
          , HE.onClick (\_ -> Restart)
          ]
          [ HH.text "Restart" ]
      , HH.button
          [ HP.class_ (HH.ClassName "next-word-btn")
          , HE.onClick (\_ -> EditWordList)
          ]
          [ HH.text "Change word list" ]
      ]
  ]

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
              { pressed: parseStroke displayedStroke
              , expected: parseStroke entry.stroke
              , hint: state.showHint
              , missing: maybe false (not <<< _.correct) state.lastStroke
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
  entry = entryAt wb state.index
  -- | An in-progress attempt takes priority; failing that, the last
  -- | resolved stroke lingers on screen until overwritten by the next
  -- | keystroke or, for a correct stroke, flashDelay elapsing.
  displayedStroke = if state.typed /= "" then state.typed else maybe "" _.stroke state.lastStroke

