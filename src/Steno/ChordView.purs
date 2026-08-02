module Steno.ChordView (chordSvg) where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Steno.Layout (GridRow(..), KeySpec, StenoKey, orderedKeys)

colWidth :: Number
colWidth = 34.0

keySize :: Number
keySize = 28.0

rowGap :: Number
rowGap = 6.0

-- | Room for the thickest key border (the hint outline, 3px) so it isn't
-- | clipped by the viewBox edge.
strokePad :: Number
strokePad = 2.0

rowTopY :: Number
rowTopY = 0.0

rowY :: GridRow -> Number
rowY gridRow = case gridRow of
  RowTop -> rowTopY
  RowBottom -> rowTopY + keySize + rowGap
  RowSpans -> rowTopY
  RowVowel -> rowTopY + 2.0 * (keySize + rowGap)

rowHeight :: GridRow -> Number
rowHeight gridRow = case gridRow of
  RowSpans -> 2.0 * keySize + rowGap
  _ -> keySize

totalWidth :: Number
totalWidth = 11.0 * colWidth + colWidth

totalHeight :: Number
totalHeight = rowY RowVowel + keySize

-- | `pressed` is what's been typed so far (or the just-completed attempt);
-- | `expected` is the current word's correct chord. A pressed key renders
-- | green if it's part of the expected chord, red otherwise. Unpressed keys
-- | stay neutral, so the answer is never revealed ahead of time — unless
-- | `hint` is on (the stuck-learner countdown), which outlines expected keys
-- | in amber, or `missing` is on (the post-submission review flash), which
-- | outlines expected-but-unpressed keys in red to call out what was left
-- | out of a wrong attempt. A key that's both missing and part of an
-- | active hint gets both classes, so its border can alternate between the
-- | two colors instead of just sitting on plain red — a missing key on its
-- | own stays static so the review flash doesn't compete for attention.
chordSvg :: forall w i. { pressed :: Set StenoKey, expected :: Set StenoKey, hint :: Boolean, missing :: Boolean } -> HH.HTML w i
chordSvg { pressed, expected, hint, missing } =
  SE.svg
    [ SA.viewBox (-strokePad) (-strokePad) (totalWidth + 2.0 * strokePad) (totalHeight + 2.0 * strokePad)
    , SA.classes [ HH.ClassName "chord-svg" ]
    ]
    (map (renderKey pressed expected hint missing) orderedKeys)

renderKey :: forall w i. Set StenoKey -> Set StenoKey -> Boolean -> Boolean -> KeySpec -> HH.HTML w i
renderKey pressed expected hint missing spec =
  SE.g []
    [ SE.rect
        [ SA.x xPos
        , SA.y yPos
        , SA.width keySize
        , SA.height height
        , SA.rx 4.0
        , SA.classes ([ HH.ClassName "steno-key", HH.ClassName stateClass ] <> outlineClass)
        ]
    , SE.text
        [ SA.x (xPos + keySize / 2.0)
        , SA.y (yPos + height / 2.0 + 4.0)
        , SA.textAnchor SA.AnchorMiddle
        , SA.fontSize (SA.FontSizeLength (SA.Px 11.0))
        , SA.classes [ HH.ClassName "steno-key-label", HH.ClassName stateClass ]
        ]
        [ HH.text spec.label ]
    ]
  where
  xPos = spec.gridCol * colWidth + colWidth / 2.0
  yPos = rowY spec.gridRow
  height = rowHeight spec.gridRow
  isPressed = Set.member spec.key pressed
  isExpected = Set.member spec.key expected
  stateClass
    | not isPressed = "idle"
    | isExpected = "correct"
    | otherwise = "incorrect"
  isMissing = missing && not isPressed && isExpected
  isHinted = hint && isExpected
  outlineClass
    | isMissing && isHinted = [ HH.ClassName "missing", HH.ClassName "hint" ]
    | isMissing = [ HH.ClassName "missing" ]
    | isHinted = [ HH.ClassName "hint" ]
    | otherwise = []
