module App.DrillApp (component) where

import Prelude

import Data.Array ((:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Steno.ChordView (chordSvg)
import Steno.Outline (parseStroke)
import Steno.Storage (loadList, saveList)
import Steno.WordBank (WordEntry, parseWordList)

type State =
  { wordBank :: Maybe (NonEmptyArray WordEntry)
  , listText :: String
  , loadError :: Maybe String
  , index :: Int
  }

data Action = Init | UpdateListText String | LoadWordList | EditWordList | NextWord

initialState :: State
initialState = { wordBank: Nothing, listText: "", loadError: Nothing, index: 0 }

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
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
  NextWord -> H.modify_ \s -> s { index = s.index + 1 }

-- | Parses and validates text as a word list. On success, saves it (so
-- | it survives a reload) and starts the drill from the top; on failure,
-- | leaves the textbox in place with an error instead of losing the
-- | learner's input.
tryLoad :: forall o m. MonadEffect m => String -> H.HalogenM State Action () o m Unit
tryLoad text = case parseWordList text of
  Left err -> H.modify_ _ { wordBank = Nothing, listText = text, loadError = Just err }
  Right wb -> do
    H.liftEffect (saveList text)
    H.put initialState { wordBank = Just wb, listText = text }

entryAt :: Int -> NonEmptyArray WordEntry -> WordEntry
entryAt index wb = fromMaybe (NEA.head wb) (NEA.index wb (index `mod` NEA.length wb))

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "drill-app") ]
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
          [ chordSvg (parseStroke entry.stroke) ]
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

