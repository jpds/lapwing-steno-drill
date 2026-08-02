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

chordSvg :: forall w i. Set StenoKey -> HH.HTML w i
chordSvg pressed =
  SE.svg
    [ SA.viewBox 0.0 0.0 totalWidth totalHeight
    , SA.classes [ HH.ClassName "chord-svg" ]
    ]
    (map (renderKey pressed) orderedKeys)

renderKey :: forall w i. Set StenoKey -> KeySpec -> HH.HTML w i
renderKey pressed spec =
  SE.g []
    [ SE.rect
        [ SA.x xPos
        , SA.y yPos
        , SA.width keySize
        , SA.height height
        , SA.rx 4.0
        , SA.classes [ HH.ClassName "steno-key", HH.ClassName stateClass ]
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
  stateClass = if Set.member spec.key pressed then "pressed" else "unpressed"
