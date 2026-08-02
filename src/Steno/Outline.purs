module Steno.Outline (parseStroke, parseStrokeStrict) where

import Prelude

import Data.Array (catMaybes, span)
import Data.Either (Either, either)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (optionMaybe, skipMany)
import Parsing.String (char, eof)
import Steno.Layout (KeySpec, Section(..), StenoKey, orderedKeys)

-- | Parse a stroke (e.g. "STPH-EU", "TP-R") into the set of keys pressed,
-- | silently ignoring anything that doesn't form a valid outline. Intended
-- | for live UI input, where a learner mid-typo shouldn't see an error.
-- | Use `parseStrokeStrict` where a malformed stroke should be caught (e.g.
-- | validating word-bank data).
parseStroke :: String -> Set StenoKey
parseStroke stroke = either (const Set.empty) identity (runParser stroke outlineParser)

-- | As `parseStroke`, but requires the whole input to be consumed as valid
-- | keys, so typos and out-of-order letters are reported instead of
-- | silently producing a different (but plausible-looking) chord.
parseStrokeStrict :: String -> Either ParseError (Set StenoKey)
parseStrokeStrict stroke = runParser stroke (outlineParser <* eof)

-- | Runs in two passes: everything up through the E/U vowels, then the
-- | right-hand consonants. Left-section keys only match while there's no
-- | "-" in the way, so a stroke like "TP-R" correctly leaves R to the
-- | right-hand pass (disambiguating it from the left-hand R) instead of
-- | letting the dash get silently skipped mid-pass.
outlineParser :: Parser String (Set StenoKey)
outlineParser = do
  before <- keysParser leftSection
  skipMany (char '-')
  after <- keysParser rightSection
  pure (Set.union before after)
  where
  { init: leftSection, rest: rightSection } =
    span (\spec -> spec.section /= RightBank) orderedKeys

keysParser :: Array KeySpec -> Parser String (Set StenoKey)
keysParser keys = Set.fromFoldable <<< catMaybes <$> traverse keyParser keys

keyParser :: KeySpec -> Parser String (Maybe StenoKey)
keyParser spec = optionMaybe (spec.key <$ char spec.char)
