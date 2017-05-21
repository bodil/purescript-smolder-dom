module Text.Smolder.Renderer.DOM
  ( render
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, Text, elementToEventTarget, elementToNode, textToNode)
import Data.CatList (CatList)
import Data.Foldable (traverse_)
import Text.Smolder.Markup (Attr(..), EventHandler(..), MarkupM(..), Markup)

foreign import makeElement :: ∀ eff. String → Eff (dom :: DOM | eff) Element
foreign import makeText :: ∀ eff. String → Eff (dom :: DOM | eff) Text

setAttr :: ∀ eff. Element → Attr → Eff (dom :: DOM | eff) Unit
setAttr n (Attr k v) = setAttribute k v n

setAttributes :: ∀ eff. Element → CatList Attr → Eff (dom :: DOM | eff) Unit
setAttributes n = traverse_ (setAttr n)

setEvent :: ∀ eff. Element → EventHandler (EventListener (dom :: DOM | eff)) → Eff (dom :: DOM | eff) Unit
setEvent n (EventHandler event handler) =
  addEventListener (EventType event) handler false $ elementToEventTarget n

-- | Render some Smolder markup into a target DOM element.
-- |
-- | Please note that this only appends the Smolder markup as new
-- | child nodes; it does not overwrite the target's current children.
render :: ∀ eff. Element → Markup (EventListener (dom :: DOM | eff)) → Eff (dom :: DOM | eff) Unit
render p (Element name children attrs events rest) = do
  el ← makeElement name
  setAttributes el attrs
  traverse_ (setEvent el) events
  render el children
  _ ← appendChild (elementToNode el) (elementToNode p)
  render p rest
render p (Content text rest) = do
  textNode ← makeText text
  _ ← appendChild (textToNode textNode) (elementToNode p)
  render p rest
render p (Return _) = pure unit
