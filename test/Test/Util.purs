module Test.Util (contains) where

import Prelude

import Data.String (Pattern(..), Replacement(..), replaceAll)

-- | Substring check via replaceAll, avoiding a direct String.contains
-- | import clash with callers that also import Data.String's contains.
contains :: String -> String -> Boolean
contains pat s = replaceAll (Pattern pat) (Replacement "") s /= s
