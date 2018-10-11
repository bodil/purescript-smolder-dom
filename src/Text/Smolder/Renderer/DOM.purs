module Text.Smolder.Renderer.DOM
  ( render
  , patch
  ) where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Safely (for_, traverse_)
import Data.Array as Array
import Data.CatList (CatList)
import Data.Foldable (class Foldable, foldrDefault)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (toUpper)
import Data.Traversable (foldMapDefaultL)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object, fromFoldable)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.Markup (Attr(..), EventHandler(..), Markup, MarkupM(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node, NodeList)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, childNodes, nodeName, nodeType, removeChild, replaceChild, setNodeValue)
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.Text (Text)
import Web.DOM.Text as Text
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener)

foreign import makeElement :: String → Effect Element
foreign import makeText :: String → Effect Text
foreign import nodeListToArray :: NodeList → Effect (Array Node)
foreign import foldlAList :: ∀ a b. (b → a → b) → b → AList a → b
foreign import patchAttributes :: Element → Object String → Effect Unit
foreign import patchEventListeners :: Element → Object EventListener → Effect Unit



-- An efficient way to cons through an array without having to convert it to a list.
newtype AList a = AList {index :: Int, array :: Array a}

uncons :: ∀ a. AList a → Maybe (Tuple a (AList a))
uncons (AList {index, array}) = case Array.index array index of
  Nothing → Nothing
  Just v → Just $ Tuple v (AList {index: index + 1, array})

instance foldableAList :: Foldable AList where
  foldl f b l = foldlAList f b l
  foldr f b l = foldrDefault f b l
  foldMap f l = foldMapDefaultL f l



setAttr :: Element → Attr → Effect Unit
setAttr n (Attr k v) = setAttribute k v n

setAttributes :: Element → CatList Attr → Effect Unit
setAttributes n = traverse_ (setAttr n)

setEvent :: Element → EventHandler EventListener → Effect Unit
setEvent n (EventHandler event handler) =
  addEventListener (EventType event) handler false $ Element.toEventTarget n

element :: String → CatList Attr → CatList (EventHandler EventListener) → Effect Element
element name attrs events = do
  el ← makeElement name
  setAttributes el attrs
  traverse_ (setEvent el) events
  pure el

-- | Render some Smolder markup into a target DOM element.
-- |
-- | Please note that this only appends the Smolder markup as new
-- | child nodes; it does not overwrite the target's current children.
render :: Element → Markup EventListener → Effect Unit
render target = foldFree (renderNode target)

renderNode :: ∀ foo. Element → MarkupM EventListener foo → Effect foo
renderNode p (Element _ name children attrs events rest) = do
  el ← element name attrs events
  render el children
  _ ← appendChild (Element.toNode el) (Element.toNode p)
  pure rest
renderNode p (Content text rest) = do
  textNode ← makeText text
  _ ← appendChild (Text.toNode textNode) (Element.toNode p)
  pure rest
renderNode p (Empty rest) = pure rest



asAList :: NodeList → Effect (AList Node)
asAList = nodeListToArray >>> map \a → (AList {index: 0, array: a})

childrenOf :: Node → Effect (AList Node)
childrenOf parent = childNodes parent >>= asAList

removeNodes :: Node → AList Node → Effect Unit
removeNodes parent children = for_ children \child → do
  _ ← removeChild child parent
  pure unit

-- | Update a DOM element's children in place.
-- |
-- | This will update existing nodes in place where possible,
-- | preserving their state. Other nodes will be created or deleted
-- | as appropriate.
-- |
-- | Please note that this function is currently not very smart -- it
-- | can't tell if a child node has moved inside its parent, and will
-- | not be able to reuse such nodes. (TODO)
patch :: Element → Markup EventListener → Effect Unit
patch parent markup = do
  let node = Element.toNode parent
  children ← childrenOf node
  childRef ← Ref.new children
  foldFree (walk node childRef) markup
  remainder ← Ref.read childRef
  removeNodes node remainder

pop :: Ref (AList Node) → Effect (Maybe Node)
pop ref = do
  Ref.modify' pop' ref
    where
      pop' l = case uncons l of
        Just (Tuple car cdr) → {state: cdr, value: Just car}
        Nothing → {state: l, value: Nothing}

walk :: Node → Ref (AList Node) → MarkupM EventListener ~> Effect
walk parent ref (Empty rest) = do
  -- this node has no children, so remove them if they exist
  children ← Ref.read ref
  removeNodes parent children
  pure rest

walk parent ref (Content text rest) = pop ref >>= \node' → case node' of
  Nothing → do
    -- add a text node past end of existing children
    textNode ← makeText text
    _ ← appendChild (Text.toNode textNode) parent
    pure rest
  Just node → do
    -- patch a text node
    let t = unsafePartial $ nodeType node
    if t == TextNode then setNodeValue text node else do
      textNode ← makeText text
      void $ replaceChild (Text.toNode textNode) node parent
    pure rest

walk parent ref (Element _ name children attrs events rest) = pop ref >>= \node' → case node' of
  Nothing → do
    -- add element past end of existing children
    el ← element name attrs events
    patch el children
    _ ← appendChild (Element.toNode el) parent
    pure rest
  Just node → do
    -- patch an element
    case toUpper (nodeName node) == toUpper name, unsafePartial $ nodeType node of
      -- current node is an element of the same name: patch it
      true, ElementNode → do
        -- this is fine, we just checked that it was an element so let's unsafeCoerce
        patchAttrs (unsafeCoerce node) attrs
        patchEvents (unsafeCoerce node) events
        patch (unsafeCoerce node) children
      -- current node isn't patchable: replace it
      _, _ → do
        el ← element name attrs events
        patch el children
        void $ replaceChild (Element.toNode el) node parent
    pure rest

patchAttrs :: Element → CatList Attr → Effect Unit
patchAttrs node attrs = patchAttributes node (fromFoldable (map toTuple attrs))
  where toTuple (Attr key value) = Tuple key value

patchEvents :: Element → CatList (EventHandler EventListener) → Effect Unit
patchEvents node events = patchEventListeners node (fromFoldable (map toTuple events))
  where toTuple (EventHandler event listener) = Tuple event listener
