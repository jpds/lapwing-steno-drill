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
-- | green if it's part of the expected chord, red otherwise; keys not yet
-- | pressed stay neutral regardless of `expected`, so the answer is never
-- | revealed ahead of time — unless `hint` is on, which outlines (but
-- | doesn't fill) the expected keys, for a learner who's stuck.
chordSvg :: forall w i. { pressed :: Set StenoKey, expected :: Set StenoKey, hint :: Boolean } -> HH.HTML w i
chordSvg { pressed, expected, hint } =
  SE.svg
    [ SA.viewBox (-strokePad) (-strokePad) (totalWidth + 2.0 * strokePad) (totalHeight + 2.0 * strokePad)
    , SA.classes [ HH.ClassName "chord-svg" ]
    ]
    (map (renderKey pressed expected hint) orderedKeys)

renderKey :: forall w i. Set StenoKey -> Set StenoKey -> Boolean -> KeySpec -> HH.HTML w i
renderKey pressed expected hint spec =
  SE.g []
    [ SE.rect
        [ SA.x xPos
        , SA.y yPos
        , SA.width keySize
        , SA.height height
        , SA.rx 4.0
        , SA.classes ([ HH.ClassName "steno-key", HH.ClassName stateClass ] <> hintClass)
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
  stateClass
    | not (Set.member spec.key pressed) = "idle"
    | Set.member spec.key expected = "correct"
    | otherwise = "incorrect"
  hintClass
    | hint && Set.member spec.key expected = [ HH.ClassName "hint" ]
    | otherwise = []
