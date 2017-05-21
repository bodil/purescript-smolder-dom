module Text.Smolder.Renderer.DOM
  ( render
  , patch
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, childNodes, nodeName, nodeType, removeChild, replaceChild, setNodeValue)
import DOM.Node.NodeList (item, length)
import DOM.Node.NodeType (NodeType(..))
import DOM.Node.Types (Element, Node, NodeList, Text, elementToEventTarget, elementToNode, textToNode)
import Data.CatList (CatList)
import Data.Foldable (for_, traverse_)
import Data.List (List(..), (..))
import Data.Maybe (fromJust)
import Data.StrMap (StrMap, fromFoldable)
import Data.String (toUpper)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Text.Smolder.Markup (Attr(..), EventHandler(..), Markup, MarkupM(..))
import Unsafe.Coerce (unsafeCoerce)

type E eff = EventListener (dom :: DOM | eff)

foreign import makeElement :: ∀ eff. String → Eff (dom :: DOM | eff) Element
foreign import makeText :: ∀ eff. String → Eff (dom :: DOM | eff) Text
foreign import patchAttributes :: ∀ eff. Element → StrMap String → Eff (dom :: DOM | eff) Unit
foreign import patchEventListeners :: ∀ eff. Element → StrMap (E eff) → Eff (dom :: DOM | eff) Unit

setAttr :: ∀ eff. Element → Attr → Eff (dom :: DOM | eff) Unit
setAttr n (Attr k v) = setAttribute k v n

setAttributes :: ∀ eff. Element → CatList Attr → Eff (dom :: DOM | eff) Unit
setAttributes n = traverse_ (setAttr n)

setEvent :: ∀ eff. Element → EventHandler (E eff) → Eff (dom :: DOM | eff) Unit
setEvent n (EventHandler event handler) =
  addEventListener (EventType event) handler false $ elementToEventTarget n

element :: ∀ eff. String → CatList Attr → CatList (EventHandler (E eff)) → Eff (dom :: DOM | eff) Element
element name attrs events = do
  el ← makeElement name
  setAttributes el attrs
  traverse_ (setEvent el) events
  pure el

-- | Render some Smolder markup into a target DOM element.
-- |
-- | Please note that this only appends the Smolder markup as new
-- | child nodes; it does not overwrite the target's current children.
render :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
render p (Element name children attrs events rest) = do
  el ← element name attrs events
  render el children
  _ ← appendChild (elementToNode el) (elementToNode p)
  render p rest
render p (Content text rest) = do
  textNode ← makeText text
  _ ← appendChild (textToNode textNode) (elementToNode p)
  render p rest
render p (Return _) = pure unit



asList :: ∀ eff. NodeList → Eff (dom :: DOM | eff) (List Node)
asList nl = do
  len ← length nl
  unsafePartial $ traverse (\i → item i nl >>= fromJust >>> pure) (0..(len - 1))

childrenOf :: ∀ eff. Node → Eff (dom :: DOM | eff) (List Node)
childrenOf parent = childNodes parent >>= asList

removeNodes :: ∀ eff. Node → List Node → Eff (dom :: DOM | eff) Unit
removeNodes parent children = for_ children \child → removeChild child parent

-- | Update a DOM element's children in place.
-- |
-- | This will update existing nodes in place where possible,
-- | preserving their state. Other nodes will be created or deleted
-- | as appropriate.
-- |
-- | Please note that this function is currently not very smart -- it
-- | can't tell if a child node has moved inside its parent, and will
-- | not be able to reuse such nodes. (TODO)
patch :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
patch parent markup = do
  let node = elementToNode parent
  children ← childrenOf node
  walk node children markup

walk :: ∀ eff. Node → List Node → Markup (E eff) → Eff (dom :: DOM | eff) Unit
walk parent Nil (Return _) =
  -- reached end of markup, no more children
  pure unit
walk parent children (Return _) =
  -- reached end of markup with child nodes left
  removeNodes parent children

walk parent Nil (Content text rest) = do
  textNode ← makeText text
  _ ← appendChild (textToNode textNode) parent
  walk parent Nil rest
walk parent (Cons node remainder) (Content text rest) = do
  let t = unsafePartial $ nodeType node
  if t == TextNode then setNodeValue text node else do
    textNode ← makeText text
    void $ replaceChild (textToNode textNode) node parent
  walk parent remainder rest

walk parent Nil (Element name children attrs events rest) = do
  el ← element name attrs events
  walk (elementToNode el) Nil children
  _ ← appendChild (elementToNode el) parent
  walk parent Nil rest
walk parent (Cons node remainder) (Element name children attrs events rest) = do
  case toUpper (nodeName node) == toUpper name, unsafePartial $ nodeType node of
    true, ElementNode → do
      -- this is fine, we just checked that it was an element
      patchAttrs (unsafeCoerce node) attrs
      patchEvents (unsafeCoerce node) events
      oldChildren ← childrenOf node
      walk node oldChildren children
    _, _ → do
      el ← element name attrs events
      walk (elementToNode el) Nil children
      void $ replaceChild (elementToNode el) node parent
  walk parent remainder rest

patchAttrs :: ∀ eff. Element → CatList Attr → Eff (dom :: DOM | eff) Unit
patchAttrs node attrs = patchAttributes node (fromFoldable (map toTuple attrs))
  where toTuple (Attr key value) = Tuple key value

patchEvents :: ∀ eff. Element → CatList (EventHandler (E eff)) → Eff (dom :: DOM | eff) Unit
patchEvents node events = patchEventListeners node (fromFoldable (map toTuple events))
  where toTuple (EventHandler event listener) = Tuple event listener
