module Steno.WordBank (WordEntry, parseWordList) where

import Prelude

import Data.Array (filter, mapMaybe, mapWithIndex, null) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), blush, hush)
import Data.Maybe (maybe)
import Data.Set as Set
import Data.String (Pattern(..), contains, split, trim)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Steno.Outline (parseStrokeStrict)

type WordEntry = { word :: String, stroke :: String }

-- | Parses a learner-pasted word list: one word/STROKE pair per line
-- | (tab-separated), blank lines ignored. Each stroke is validated with
-- | parseStrokeStrict up front (same check the round-trip tests used to
-- | run against the hardcoded word bank) so a typo shows up as a clear
-- | error message instead of a silently-broken drill entry later.
parseWordList :: String -> Either String (NonEmptyArray WordEntry)
parseWordList input =
  if not (Array.null errors) then Left (joinWith "\n" errors)
  else maybe (Left "No word list entries found.") Right (NEA.fromArray entries)
  where
  parsedLines =
    map parseLine
      $ Array.filter (\(Tuple _ line) -> trim line /= "")
      $ Array.mapWithIndex (\i line -> Tuple (i + 1) line) (split (Pattern "\n") input)
  errors = Array.mapMaybe blush parsedLines
  entries = Array.mapMaybe hush parsedLines

parseLine :: Tuple Int String -> Either String WordEntry
parseLine (Tuple lineNumber line) =
  case split (Pattern "\t") (trim line) of
    [ word, rawStroke ] | trim word /= "" && trim rawStroke /= "" ->
      validateStroke lineNumber (trim word) (trim rawStroke)
    _ ->
      Left (errorPrefix lineNumber <> "expected \"word<TAB>STROKE\", got: " <> line)

validateStroke :: Int -> String -> String -> Either String WordEntry
validateStroke lineNumber word stroke
  | contains (Pattern "/") stroke =
      Left (errorPrefix lineNumber <> "multi-stroke outline \"" <> stroke <> "\" isn't supported yet")
  | otherwise =
      case parseStrokeStrict stroke of
        Left _ -> Left (errorPrefix lineNumber <> "invalid stroke \"" <> stroke <> "\" (not a valid single-stroke outline - check spelling and key order)")
        Right keys | Set.isEmpty keys -> Left (errorPrefix lineNumber <> "stroke \"" <> stroke <> "\" has no keys")
        Right _ -> Right { word, stroke }

errorPrefix :: Int -> String
errorPrefix lineNumber = "Line " <> show lineNumber <> ": "

