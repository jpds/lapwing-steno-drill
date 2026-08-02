module Steno.Style (stylesheet) where

import Prelude

import CSS
  ( CSS
  , Feature(..)
  , Key
  , MediaType(..)
  , Value
  , alignItems
  , borderRadius
  , cursor
  , display
  , flex
  , flexDirection
  , fontFamily
  , fontSize
  , height
  , key
  , margin
  , maxWidth
  , padding
  , pct
  , px
  , query
  , rem
  , sansSerif
  , select
  , width
  )
import CSS.Common (auto, center)
import CSS.Cursor (pointer)
import CSS.Flexbox (column)
import CSS.Font (monospace, systemUi)
import CSS.Render (render, renderedSheet)
import CSS.String (fromString)
import CSS.TextAlign as TextAlign
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))

stylesheet :: String
stylesheet = fromMaybe "" (renderedSheet (render css))

-- | purescript-css has no notion of CSS custom properties (`--x`) or the
-- | `var()` function, so declaring and referencing them drops to the raw
-- | `key`/`Value` escape hatch. Used only where no typed combinator exists.
prop :: String -> String -> CSS
prop name val = key (fromString name :: Key Value) (fromString val :: Value)

varProp :: String -> String -> CSS
varProp name varName = prop name ("var(" <> varName <> ")")

root :: CSS -> CSS
root = select (fromString ":root")

css :: CSS
css = do
  root lightTheme
  query (MediaType (fromString "all"))
    (Feature "prefers-color-scheme" (Just (fromString "dark")) :| [])
    (root darkTheme)
  bodyRule
  drillColumnsRule
  wordPanelRule
  chordViewRule
  chordSvgSvgRule
  drillCompleteRule
  strokeCaptureRule
  drillActionsRule
  actionBtnRule
  listInputRule
  listErrorRule
  keyRules

lightTheme :: CSS
lightTheme = do
  prop "--bg" "#ffffff"
  prop "--fg" "#1a1a1a"
  prop "--key-bg" "#dae0f5"
  prop "--key-border" "#506477"
  prop "--key-text" "#506477"
  prop "--key-correct-bg" "#4caf50"
  prop "--key-correct-text" "#0f2b10"
  prop "--key-incorrect-bg" "#e05252"
  prop "--key-incorrect-text" "#2b0f0f"
  prop "--key-hint-border" "#f0ad4e"

darkTheme :: CSS
darkTheme = do
  prop "--bg" "#16181d"
  prop "--fg" "#e8e8e8"
  prop "--key-bg" "#2a2f3a"
  prop "--key-border" "#6b7688"
  prop "--key-text" "#b7c0cc"
  prop "--key-correct-bg" "#4caf50"
  prop "--key-correct-text" "#0f2b10"
  prop "--key-incorrect-bg" "#e05252"
  prop "--key-incorrect-text" "#2b0f0f"
  prop "--key-hint-border" "#f0ad4e"

bodyRule :: CSS
bodyRule = select (fromString "body") do
  fontFamily [] (systemUi :| [ sansSerif ])
  maxWidth (px 760.0)
  margin (rem 2.0) auto auto auto
  padding (px 0.0) (rem 1.0) (px 0.0) (rem 1.0)
  varProp "background" "--bg"
  varProp "color" "--fg"

drillColumnsRule :: CSS
drillColumnsRule = select (fromString ".drill-columns") do
  display flex
  flexDirection column
  alignItems center
  prop "gap" "1.5rem"

wordPanelRule :: CSS
wordPanelRule = select (fromString ".word-panel") do
  fontSize (rem 3.0)
  fontFamily [] (monospace :| [])
  TextAlign.textAlign TextAlign.center

chordViewRule :: CSS
chordViewRule = select (fromString ".chord-view") do
  width (pct 100.0)
  maxWidth (px 680.0)

chordSvgSvgRule :: CSS
chordSvgSvgRule = select (fromString ".chord-view svg") do
  width (pct 100.0)
  height auto

drillCompleteRule :: CSS
drillCompleteRule = select (fromString ".drill-complete") do
  fontSize (rem 1.5)
  TextAlign.textAlign TextAlign.center
  prop "margin" "2rem 0"

-- | This input is what actually receives typed strokes (see App.DrillApp),
-- | but the chord diagram already shows live feedback, so there's no need
-- | to show the raw text too. Kept focusable (not display:none) via the
-- | standard visually-hidden pattern rather than being a visible text box.
strokeCaptureRule :: CSS
strokeCaptureRule = select (fromString ".stroke-capture") do
  prop "position" "absolute"
  width (px 1.0)
  height (px 1.0)
  padding (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  margin (px (-1.0)) (px (-1.0)) (px (-1.0)) (px (-1.0))
  prop "overflow" "hidden"
  prop "clip" "rect(0, 0, 0, 0)"
  prop "white-space" "nowrap"
  prop "border" "0"

drillActionsRule :: CSS
drillActionsRule = select (fromString ".drill-actions") do
  display flex
  prop "justify-content" "center"
  prop "gap" "1rem"
  prop "margin-top" "1.5rem"

actionBtnRule :: CSS
actionBtnRule = select (fromString ".next-word-btn") do
  padding (rem 0.5) (rem 1.5) (rem 0.5) (rem 1.5)
  fontSize (rem 1.0)
  varProp "background" "--key-bg"
  varProp "color" "--key-text"
  prop "border" "1px solid var(--key-border)"
  borderRadius (px 6.0) (px 6.0) (px 6.0) (px 6.0)
  cursor pointer

listInputRule :: CSS
listInputRule = select (fromString ".list-input") do
  width (pct 100.0)
  fontFamily [] (monospace :| [])
  fontSize (rem 1.0)
  padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
  varProp "background" "--bg"
  varProp "color" "--fg"
  prop "border" "1px solid var(--key-border)"
  borderRadius (px 6.0) (px 6.0) (px 6.0) (px 6.0)

listErrorRule :: CSS
listErrorRule = select (fromString ".list-error") do
  fontFamily [] (monospace :| [])
  fontSize (rem 0.9)
  varProp "color" "--key-incorrect-bg"
  prop "white-space" "pre-wrap"

keyRules :: CSS
keyRules = do
  select (fromString ".steno-key") do
    varProp "stroke" "--key-border"
    prop "stroke-width" "1.5"
    varProp "fill" "--key-bg"
    prop "transition" "stroke 0.15s ease, stroke-width 0.15s ease"
  select (fromString ".steno-key.correct") do
    varProp "fill" "--key-correct-bg"
  select (fromString ".steno-key.incorrect") do
    varProp "fill" "--key-incorrect-bg"
  select (fromString ".steno-key.hint") do
    varProp "stroke" "--key-hint-border"
    prop "stroke-width" "3"
  select (fromString ".steno-key-label") do
    varProp "fill" "--key-text"
  select (fromString ".steno-key-label.correct") do
    varProp "fill" "--key-correct-text"
  select (fromString ".steno-key-label.incorrect") do
    varProp "fill" "--key-incorrect-text"
