module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Steno.WordBank as WordBank

main :: Effect Unit
main = do
  WordBank.main
  log "All word list parser tests passed."
