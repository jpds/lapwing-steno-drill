module Test.Steno.WordBank (main) where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Steno.Layout (StenoKey, orderedKeys)
import Steno.Outline (parseStrokeStrict)
import Steno.WordBank (WordEntry, parseWordList)
import Test.Assert (assertEqual')
import Test.Util (contains)

main :: Effect Unit
main = do
  testValidList
  testBlankLinesIgnored
  testRejectsMalformedLine
  testLineNumberingSkipsBlankLines
  testAcceptsCrlfLineEndings
  testRejectsInvalidStroke
  testRejectsEmptyStroke
  testRejectsMultiStrokeOutline
  testRejectsEmptyInput
  testReportsAllBadLines

-- | Every entry in a successfully-parsed list must itself round-trip
-- | through parseStrokeStrict (this is the same check that used to run
-- | against the hardcoded word bank), catching typos or out-of-order
-- | letters in whatever the learner pastes.
testValidList :: Effect Unit
testValidList = case parseWordList "the\t-T\nof\t-F\nand\tSKP" of
  Left err -> assertEqual' ("expected a valid parse, got error: " <> err) { actual: false, expected: true }
  Right entries -> do
    assertEqual' "entry count" { actual: NEA.length entries, expected: 3 }
    for_ (NEA.toArray entries) checkEntry

testBlankLinesIgnored :: Effect Unit
testBlankLinesIgnored = case parseWordList "the\t-T\n\n\nof\t-F\n" of
  Left err -> assertEqual' ("expected blank lines to be skipped, got error: " <> err) { actual: false, expected: true }
  Right entries -> assertEqual' "entry count with blank lines" { actual: NEA.length entries, expected: 2 }

testRejectsMalformedLine :: Effect Unit
testRejectsMalformedLine = case parseWordList "the\t-T\nnotabtabbed" of
  Left err -> assertEqual' "error mentions the offending line" { actual: contains "Line 2" err, expected: true }
  Right _ -> assertEqual' "expected a malformed line to be rejected" { actual: true, expected: false }

testLineNumberingSkipsBlankLines :: Effect Unit
testLineNumberingSkipsBlankLines = case parseWordList "\n\n\nbad\tXYZ123" of
  Left err -> assertEqual' "error references the physical line, not the post-filter index" { actual: contains "Line 4" err, expected: true }
  Right _ -> assertEqual' "expected an invalid stroke to be rejected" { actual: true, expected: false }

testAcceptsCrlfLineEndings :: Effect Unit
testAcceptsCrlfLineEndings = case parseWordList "the\t-T\r\nof\t-F\r\n" of
  Left err -> assertEqual' ("expected CRLF input to parse, got error: " <> err) { actual: false, expected: true }
  Right entries -> assertEqual' "entry count with CRLF line endings" { actual: NEA.length entries, expected: 2 }

testRejectsInvalidStroke :: Effect Unit
testRejectsInvalidStroke = case parseWordList "the\tXYZ123" of
  Left _ -> pure unit
  Right _ -> assertEqual' "expected an invalid stroke to be rejected" { actual: true, expected: false }

testRejectsEmptyStroke :: Effect Unit
testRejectsEmptyStroke = case parseWordList "the\t-" of
  Left _ -> pure unit
  Right _ -> assertEqual' "expected a stroke with no keys to be rejected" { actual: true, expected: false }

testRejectsMultiStrokeOutline :: Effect Unit
testRejectsMultiStrokeOutline = case parseWordList "faculty\tTPA/KULT" of
  Left _ -> pure unit
  Right _ -> assertEqual' "expected a multi-stroke outline to be rejected" { actual: true, expected: false }

testRejectsEmptyInput :: Effect Unit
testRejectsEmptyInput = case parseWordList "\n\n  \n" of
  Left _ -> pure unit
  Right _ -> assertEqual' "expected empty input to be rejected" { actual: true, expected: false }

testReportsAllBadLines :: Effect Unit
testReportsAllBadLines = case parseWordList "bad1\tXYZ\nbad2\tABC" of
  Left err -> do
    assertEqual' "reports the first bad line" { actual: contains "Line 1" err, expected: true }
    assertEqual' "also reports the second bad line, not just the first" { actual: contains "Line 2" err, expected: true }
  Right _ -> assertEqual' "expected both invalid strokes to be rejected" { actual: true, expected: false }

checkEntry :: WordEntry -> Effect Unit
checkEntry entry = case parseStrokeStrict entry.stroke of
  Left err ->
    assertEqual'
      ("\"" <> entry.word <> "\" (" <> entry.stroke <> ") failed to parse: " <> show err)
      { actual: false, expected: true }
  Right keys ->
    assertEqual'
      ("\"" <> entry.word <> "\" (" <> entry.stroke <> ") round-trip")
      { actual: serialize keys, expected: stripDashes entry.stroke }

serialize :: Set StenoKey -> String
serialize keys =
  fromCharArray (map _.char (filter (\spec -> Set.member spec.key keys) orderedKeys))

stripDashes :: String -> String
stripDashes = replaceAll (Pattern "-") (Replacement "")
