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
  testAcceptsMultiStrokeOutline
  testRejectsInvalidSegment
  testRejectsEmptySegment
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

testAcceptsMultiStrokeOutline :: Effect Unit
testAcceptsMultiStrokeOutline = case parseWordList "faculty\tTPA/KULT" of
  Left err -> assertEqual' ("expected a multi-stroke outline to parse, got error: " <> err) { actual: false, expected: true }
  Right entries -> case NEA.toArray entries of
    [ entry ] -> do
      assertEqual' "entry word" { actual: entry.word, expected: "faculty" }
      assertEqual' "entry strokes" { actual: NEA.toArray entry.strokes, expected: [ "TPA", "KULT" ] }
    _ -> assertEqual' "expected exactly one entry" { actual: true, expected: false }

testRejectsInvalidSegment :: Effect Unit
testRejectsInvalidSegment = case parseWordList "faculty\tTPA/XYZ123" of
  Left err -> assertEqual' "error mentions the offending segment" { actual: contains "XYZ123" err, expected: true }
  Right _ -> assertEqual' "expected an invalid segment to be rejected" { actual: true, expected: false }

testRejectsEmptySegment :: Effect Unit
testRejectsEmptySegment = case parseWordList "faculty\tTPA/" of
  Left err -> assertEqual' "error mentions line 1" { actual: contains "Line 1" err, expected: true }
  Right _ -> assertEqual' "expected an empty trailing segment to be rejected" { actual: true, expected: false }

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
checkEntry entry = for_ (NEA.toArray entry.strokes) (checkSegment entry.word)

checkSegment :: String -> String -> Effect Unit
checkSegment word stroke = case parseStrokeStrict stroke of
  Left err ->
    assertEqual'
      ("\"" <> word <> "\" (" <> stroke <> ") failed to parse: " <> show err)
      { actual: false, expected: true }
  Right keys ->
    assertEqual'
      ("\"" <> word <> "\" (" <> stroke <> ") round-trip")
      { actual: serialize keys, expected: stripDashes stroke }

serialize :: Set StenoKey -> String
serialize keys =
  fromCharArray (map _.char (filter (\spec -> Set.member spec.key keys) orderedKeys))

stripDashes :: String -> String
stripDashes = replaceAll (Pattern "-") (Replacement "")
