module Steno.Storage (loadList, saveList) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

-- | The learner's pasted word list, persisted so a page reload doesn't
-- | dump them back at the empty textbox. Storage access can throw (e.g.
-- | Safari private mode, a full quota), which must never be allowed to
-- | block the drill itself - persistence is a nice-to-have, so failures
-- | are swallowed rather than surfaced.
storageKey :: String
storageKey = "lapwing-steno-drill/word-list"

loadList :: Effect (Maybe String)
loadList = join <<< hush <$> try (getItem storageKey =<< localStorage =<< window)

saveList :: String -> Effect Unit
saveList text = void $ try (setItem storageKey text =<< localStorage =<< window)

