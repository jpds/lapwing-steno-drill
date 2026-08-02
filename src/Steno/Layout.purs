module Steno.Layout
  ( StenoKey(..)
  , KeySpec
  , GridRow(..)
  , Section(..)
  , orderedKeys
  , keySpec
  ) where

import Prelude

-- | Stroke order matters: this is the order keys must appear in a written outline.
data StenoKey
  = Num
  | LS
  | LT
  | LK
  | LP
  | LW
  | LH
  | LR
  | LA
  | LO
  | Star
  | RE
  | RU
  | RF
  | RR
  | RP
  | RB
  | RL
  | RG
  | RT
  | RS
  | RD
  | RZ

derive instance eqStenoKey :: Eq StenoKey
derive instance ordStenoKey :: Ord StenoKey

-- | Which physical row a key sits on. Consonants split into a top and a
-- | bottom row per hand (e.g. left hand: T/P/H on top, K/W/R on bottom).
data GridRow
  = RowTop
  | RowBottom
  | RowSpans
  | RowVowel

-- | Which side of the board a key belongs to. Outline.purs uses this to
-- | decide which keys a "-" can disambiguate between.
data Section
  = LeftBank
  | Vowels
  | RightBank

derive instance eqSection :: Eq Section

type KeySpec =
  { key :: StenoKey
  , char :: Char
  , label :: String
  , gridCol :: Number
  , gridRow :: GridRow
  , section :: Section
  }

-- | Total mapping from every `StenoKey` to its grid position (mirroring the
-- | physical steno keyboard: left hand consonants in columns 0-3, T/P/H top
-- | and K/W/R bottom, S on the bottom row; right hand consonants in columns
-- | 6-10, F/P/L/T/D top and R/B/G/S/Z bottom; vowels below; star spanning
-- | both rows in its own column just left of the right hand) and stroke
-- | character. Missing a constructor here is a non-exhaustive-pattern
-- | compiler warning, not a silently empty entry.
keySpec :: StenoKey -> KeySpec
keySpec = case _ of
  Num -> { key: Num, char: '#', label: "#", gridCol: 0.0, gridRow: RowTop, section: LeftBank }
  LS -> { key: LS, char: 'S', label: "S", gridCol: 0.0, gridRow: RowBottom, section: LeftBank }
  LT -> { key: LT, char: 'T', label: "T", gridCol: 1.0, gridRow: RowTop, section: LeftBank }
  LK -> { key: LK, char: 'K', label: "K", gridCol: 1.0, gridRow: RowBottom, section: LeftBank }
  LP -> { key: LP, char: 'P', label: "P", gridCol: 2.0, gridRow: RowTop, section: LeftBank }
  LW -> { key: LW, char: 'W', label: "W", gridCol: 2.0, gridRow: RowBottom, section: LeftBank }
  LH -> { key: LH, char: 'H', label: "H", gridCol: 3.0, gridRow: RowTop, section: LeftBank }
  LR -> { key: LR, char: 'R', label: "R", gridCol: 3.0, gridRow: RowBottom, section: LeftBank }
  LA -> { key: LA, char: 'A', label: "A", gridCol: 2.0, gridRow: RowVowel, section: Vowels }
  LO -> { key: LO, char: 'O', label: "O", gridCol: 3.0, gridRow: RowVowel, section: Vowels }
  Star -> { key: Star, char: '*', label: "*", gridCol: 5.0, gridRow: RowSpans, section: Vowels }
  RE -> { key: RE, char: 'E', label: "E", gridCol: 6.0, gridRow: RowVowel, section: Vowels }
  RU -> { key: RU, char: 'U', label: "U", gridCol: 7.0, gridRow: RowVowel, section: Vowels }
  RF -> { key: RF, char: 'F', label: "F", gridCol: 6.0, gridRow: RowTop, section: RightBank }
  RR -> { key: RR, char: 'R', label: "R", gridCol: 6.0, gridRow: RowBottom, section: RightBank }
  RP -> { key: RP, char: 'P', label: "P", gridCol: 7.0, gridRow: RowTop, section: RightBank }
  RB -> { key: RB, char: 'B', label: "B", gridCol: 7.0, gridRow: RowBottom, section: RightBank }
  RL -> { key: RL, char: 'L', label: "L", gridCol: 8.0, gridRow: RowTop, section: RightBank }
  RG -> { key: RG, char: 'G', label: "G", gridCol: 8.0, gridRow: RowBottom, section: RightBank }
  RT -> { key: RT, char: 'T', label: "T", gridCol: 9.0, gridRow: RowTop, section: RightBank }
  RS -> { key: RS, char: 'S', label: "S", gridCol: 9.0, gridRow: RowBottom, section: RightBank }
  RD -> { key: RD, char: 'D', label: "D", gridCol: 10.0, gridRow: RowTop, section: RightBank }
  RZ -> { key: RZ, char: 'Z', label: "Z", gridCol: 10.0, gridRow: RowBottom, section: RightBank }

-- | All keys, in stroke order. Kept as an explicit list (rather than derived
-- | via Bounded/Enum) since that's the only place stroke order is encoded.
allKeys :: Array StenoKey
allKeys =
  [ Num
  , LS
  , LT
  , LK
  , LP
  , LW
  , LH
  , LR
  , LA
  , LO
  , Star
  , RE
  , RU
  , RF
  , RR
  , RP
  , RB
  , RL
  , RG
  , RT
  , RS
  , RD
  , RZ
  ]

orderedKeys :: Array KeySpec
orderedKeys = map keySpec allKeys
