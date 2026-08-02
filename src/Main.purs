module Main (main) where

import Prelude

import App.DrillApp (component)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Steno.Style (stylesheet)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  injectStylesheet
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

injectStylesheet :: Effect Unit
injectStylesheet = do
  doc <- window >>= Window.document
  maybeHead <- HTMLDocument.head doc
  case maybeHead of
    Nothing -> pure unit
    Just headEl -> do
      styleEl <- Document.createElement "style" (HTMLDocument.toDocument doc)
      let styleNode = Element.toNode styleEl
      Node.setTextContent stylesheet styleNode
      Node.appendChild styleNode (HTMLElement.toNode headEl)
