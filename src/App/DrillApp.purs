module App.DrillApp (component) where

import Prelude

import Data.Array (intersperse, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (contains, length)
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
import Unsafe.Reference (unsafeRefEq)
import Web.HTML (window)
import Web.HTML.HTMLDocument (activeElement)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window (document)

type State =
  { wordBank :: Maybe (NonEmptyArray WordEntry)
  , listText :: String
  , loadError :: Maybe String
  , index :: Int
  , strokeIndex :: Int
  , typed :: String
  , showHint :: Boolean
  , completed :: Boolean
  , lastStroke :: Maybe { stroke :: String, correct :: Boolean }
  , pendingAdvance :: Maybe { index :: Int, strokeIndex :: Int, isLastSegment :: Boolean }
  , inputFocused :: Boolean
  , showFocusWarning :: Boolean
  , focusEpoch :: Int
  }

data Action
  = Init
  | UpdateListText String
  | LoadWordList
  | EditWordList
  | HandleInput String
  | NextWord
  | AutoHint Int Int
  | RefocusInput
  | InputFocusChanged Boolean
  | Restart

initialState :: State
initialState =
  { wordBank: Nothing
  , listText: ""
  , loadError: Nothing
  , index: 0
  , strokeIndex: 0
  , typed: ""
  , showHint: false
  , completed: false
  , lastStroke: Nothing
  , pendingAdvance: Nothing
  , inputFocused: true
  , showFocusWarning: false
  , focusEpoch: 0
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
-- | one advances to the next segment or word (see `catchUpAdvance`).
flashDelay :: Milliseconds
flashDelay = Milliseconds 150.0

-- | How long the input must stay unfocused before the word blurs -
-- | clicking any button in this app blurs the hidden input for a tick
-- | before `focusInput` reclaims it, so a short debounce keeps that from
-- | flashing the blur on every click.
focusWarningDelay :: Milliseconds
focusWarningDelay = Milliseconds 400.0

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

-- | `pendingAdvance` (not `lastStroke`) tracks an owed transition, since
-- | `lastStroke` gets cleared by the next stroke's own intermediate
-- | keystrokes before its delimiter arrives. Every keystroke catches up
-- | any pending transition first, so it's always judged against the true
-- | current position rather than a stale one still waiting on its timer.
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
    state <- H.get
    H.put initialState
      { listText = state.listText
      , inputFocused = state.inputFocused
      , showFocusWarning = state.showFocusWarning
      , focusEpoch = state.focusEpoch
      }
    focusInput
  HandleInput v -> do
    beforeCatchUp <- H.get
    if beforeCatchUp.completed then
      pure unit
    else do
      when (isJust beforeCatchUp.pendingAdvance) catchUpAdvance
      state <- H.get
      if state.completed then
        pure unit
      else if contains (Pattern strokeDelimiter) v then do
        let stroke = takeWhile (_ /= ' ') v
        entry <- H.gets currentEntry
        case entry of
          Nothing -> pure unit
          Just e -> do
            let correct = parseStroke stroke == parseStroke (segmentAt e state.strokeIndex)
            let isLastSegment = state.strokeIndex + 1 >= NEA.length e.strokes
            let advance = { index: state.index, strokeIndex: state.strokeIndex, isLastSegment }
            H.modify_ _ { typed = "", lastStroke = Just { stroke, correct } }
            if correct then do
              H.modify_ _ { pendingAdvance = Just advance }
              void $ H.fork do
                H.liftAff (Aff.delay flashDelay)
                cur <- H.gets _.pendingAdvance
                when (cur == Just advance) catchUpAdvance
            else
              focusInput
      else
        H.modify_ _ { typed = v, lastStroke = Nothing }
  NextWord -> do
    H.modify_ _ { typed = "", lastStroke = Nothing, pendingAdvance = Nothing }
    advanceWord
  AutoHint forIndex forStrokeIndex -> do
    state <- H.get
    -- a timer armed while focused can still fire after focus is lost
    -- (see `scheduleHint`), so this re-checks focus, not just position
    let stillCurrent = state.index == forIndex && state.strokeIndex == forStrokeIndex
    when (stillCurrent && not state.completed && state.inputFocused) $
      H.modify_ _ { showHint = true }
  RefocusInput -> focusInput
  InputFocusChanged true -> do
    H.modify_ _ { inputFocused = true, showFocusWarning = false }
    state <- H.get
    when (isJust state.wordBank && not state.completed && not state.showHint) scheduleHint
  InputFocusChanged false -> do
    state <- H.get
    let epoch = state.focusEpoch + 1
    H.modify_ _ { inputFocused = false, focusEpoch = epoch }
    void $ H.fork do
      H.liftAff (Aff.delay focusWarningDelay)
      cur <- H.get
      -- `epoch` guards against a stale fork from an earlier, already-ended
      -- blur outliving a more recent one (same pattern as `pendingAdvance`)
      when (not cur.inputFocused && cur.focusEpoch == epoch) $
        H.modify_ _ { showFocusWarning = true, showHint = false }
  Restart -> do
    state <- H.get
    reshuffled <- H.liftEffect (traverse shuffle state.wordBank)
    H.put initialState
      { wordBank = reshuffled
      , listText = state.listText
      , inputFocused = state.inputFocused
      , showFocusWarning = state.showFocusWarning
      , focusEpoch = state.focusEpoch
      }
    scheduleHint
    focusInput

-- | Applies whatever transition `pendingAdvance` says is owed, and clears
-- | it. A no-op if there's nothing pending, so it's safe to call from
-- | both the delayed fork and an early-arriving next stroke - whichever
-- | runs first wins.
catchUpAdvance :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
catchUpAdvance = do
  state <- H.get
  case state.pendingAdvance of
    Nothing -> pure unit
    Just pending -> do
      H.modify_ _ { lastStroke = Nothing, pendingAdvance = Nothing }
      if pending.isLastSegment then
        advanceWord
      else do
        H.modify_ \s -> s { strokeIndex = s.strokeIndex + 1, showHint = false }
        scheduleHint

-- | Moves to the next word (or completes the drill).
advanceWord :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
advanceWord = do
  state <- H.get
  case state.wordBank of
    Nothing -> pure unit
    Just wb -> do
      let newIndex = state.index + 1
      H.modify_ _ { index = newIndex, strokeIndex = 0, typed = "", showHint = false }
      if newIndex >= NEA.length wb then
        H.modify_ _ { completed = true }
      else do
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
    state <- H.get
    H.put initialState
      { wordBank = Just shuffled
      , listText = text
      , inputFocused = state.inputFocused
      , showFocusWarning = state.showFocusWarning
      , focusEpoch = state.focusEpoch
      }
    scheduleHint
    focusInput

-- | `.focus()` can silently fail (e.g. the browser window isn't focused),
-- | and `onFocus`/`onBlur` only fire on an actual focus change - so this
-- | verifies against `document.activeElement` rather than assuming success.
focusInput :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
focusInput = do
  el <- H.getHTMLElementRef inputRef
  for_ el \element -> do
    H.liftEffect (focus element)
    actuallyFocused <- H.liftEffect do
      doc <- document =<< window
      active <- activeElement doc
      pure (maybe false (unsafeRefEq element) active)
    state <- H.get
    when (actuallyFocused /= state.inputFocused) $
      handleAction (InputFocusChanged actuallyFocused)

-- | Schedules the hint reveal for the current word/segment; the captured
-- | position guards against a stale hint if it's advanced by the time this
-- | fires. Skips arming while unfocused, since strokes aren't reaching the
-- | app - `InputFocusChanged` restarts it on refocus, and `AutoHint`
-- | re-checks focus in case it's lost mid-countdown.
scheduleHint :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
scheduleHint = do
  state <- H.get
  when state.inputFocused $
    void $ H.fork do
      H.liftAff (Aff.delay hintDelay)
      handleAction (AutoHint state.index state.strokeIndex)

-- | The word bank always loops via mod, and NextWord marks the drill
-- | completed the instant index would run past the end, so this
-- | fromMaybe fallback is unreachable in practice; it just keeps
-- | NEA.index total without threading a partiality proof through.
entryAt :: NonEmptyArray WordEntry -> Int -> WordEntry
entryAt wb index = fromMaybe (NEA.head wb) (NEA.index wb (index `mod` NEA.length wb))

currentEntry :: State -> Maybe WordEntry
currentEntry state = (\wb -> entryAt wb state.index) <$> state.wordBank

-- | As `entryAt`: `strokeIndex` is always kept in range by `HandleInput`,
-- | so the fallback is unreachable in practice.
segmentAt :: WordEntry -> Int -> String
segmentAt entry strokeIndex = fromMaybe (NEA.head entry.strokes) (NEA.index entry.strokes strokeIndex)

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
      [ HP.class_ (HH.ClassName "drill-progress") ]
      [ HH.text (show (state.index + 1) <> " / " <> show (NEA.length wb)) ]
  , HH.div
      [ HP.class_ (HH.ClassName "drill-columns") ]
      [ HH.div
          [ HP.class_ (HH.ClassName ("word-panel" <> wordPanelStateClass)) ]
          [ HH.text entry.word ]
      , outlineProgress
      , HH.div
          [ HP.class_ (HH.ClassName "chord-view") ]
          [ chordSvg
              { pressed: parseStroke displayedStroke
              , expected: parseStroke (segmentAt entry state.strokeIndex)
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
      , HE.onFocus (\_ -> InputFocusChanged true)
      , HE.onBlur (\_ -> InputFocusChanged false)
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
  -- | The stroke-capture input is visually hidden, so a lost-focus state
  -- | (which silently stops all strokes) would otherwise have no on-screen
  -- | sign - blurring the word itself doubles as the "click to resume" cue,
  -- | since the whole app already refocuses the input on any click.
  wordPanelStateClass
    | state.showFocusWarning = " unfocused"
    | wordFlash = " correct"
    | otherwise = ""
  -- | An in-progress attempt takes priority; failing that, the last
  -- | resolved stroke lingers until overwritten or cleared (see `catchUpAdvance`).
  displayedStroke = if state.typed /= "" then state.typed else maybe "" _.stroke state.lastStroke
  isLastSegment = state.strokeIndex + 1 >= NEA.length entry.strokes
  -- | The whole word flashes green only once its final segment resolves
  -- | correctly - a single-stroke word's only segment is always "last",
  -- | so a multi-stroke outline only flashes the word once fully typed.
  wordFlash = isLastSegment && maybe false _.correct state.lastStroke
  outlineProgress
    | NEA.length entry.strokes <= 1 = HH.text ""
    | otherwise =
        HH.div
          [ HP.class_ (HH.ClassName "outline-progress") ]
          ( intersperse outlineSeparator
              (NEA.toArray (NEA.mapWithIndex renderSegment entry.strokes))
          )
  outlineSeparator =
    HH.span [ HP.class_ (HH.ClassName "outline-separator") ] [ HH.text "/" ]
  -- | Reserves each bubble's full width for its own letters up front (as
  -- | an inline style, since it's per-segment), so revealing them never
  -- | resizes the bubble - only its text content changes.
  renderSegment i segment =
    HH.span
      [ HP.class_ (HH.ClassName ("outline-segment" <> segmentClass i))
      , HP.style ("min-width: max(20px, " <> show (length segment) <> "ch)")
      ]
      [ HH.text segment ]
  isDone i = i < state.strokeIndex || (i == state.strokeIndex && maybe false _.correct state.lastStroke)
  segmentClass i
    | isDone i = " segment-done"
    | i == state.strokeIndex = " segment-current"
    | otherwise = ""
