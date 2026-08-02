module Steno.Shuffle (shuffle) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Random (randomInt)

-- | Fisher-Yates shuffle. Word lists are at most a few hundred entries, so
-- | the O(n^2) cost of repeated `deleteAt` is not worth optimizing away
-- | with an ST array.
shuffle :: forall a. NonEmptyArray a -> Effect (NonEmptyArray a)
shuffle entries = do
  shuffled <- shuffleArray (NEA.toArray entries)
  pure (fromMaybe entries (NEA.fromArray shuffled))

shuffleArray :: forall a. Array a -> Effect (Array a)
shuffleArray xs
  | Array.null xs = pure xs
  | otherwise = do
      i <- randomInt 0 (Array.length xs - 1)
      case Array.index xs i, Array.deleteAt i xs of
        Just picked, Just rest -> Array.cons picked <$> shuffleArray rest
        _, _ -> pure xs
