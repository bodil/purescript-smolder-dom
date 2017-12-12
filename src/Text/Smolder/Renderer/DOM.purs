module Text.Smolder.Renderer.DOM
  ( render
  , patch
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef', newRef, readRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Free (foldFree)
import Control.Safely (for_, traverse_)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, childNodes, nodeName, nodeType, removeChild, replaceChild, setNodeValue)
import DOM.Node.NodeType (NodeType(..))
import DOM.Node.Types (Element, Node, NodeList, Text, elementToEventTarget, elementToNode, textToNode)
import Data.Array as Array
import Data.CatList (CatList)
import Data.Foldable (class Foldable, foldrDefault)
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (StrMap, fromFoldable)
import Data.String (toUpper)
import Data.Traversable (foldMapDefaultL)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Text.Smolder.Markup (NS(..), Attr(..), EventHandler(..), Markup, MarkupM(..))
import Unsafe.Coerce (unsafeCoerce)

type E eff = EventListener (dom :: DOM | eff)

foreign import makeElement :: ∀ eff. String → Eff (dom :: DOM | eff) Element
foreign import makeElementNS :: ∀ eff. String → String → Eff (dom :: DOM | eff) Element
foreign import makeText :: ∀ eff. String → Eff (dom :: DOM | eff) Text
foreign import nodeListToArray :: ∀ eff. NodeList → Eff (dom :: DOM | eff) (Array Node)
foreign import foldlAList :: ∀ a b. (b → a → b) → b → AList a → b
foreign import patchAttributes :: ∀ eff. Element → StrMap String → Eff (dom :: DOM | eff) Unit
foreign import patchEventListeners :: ∀ eff. Element → StrMap (E eff) → Eff (dom :: DOM | eff) Unit



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



setAttr :: ∀ eff. Element → Attr → Eff (dom :: DOM | eff) Unit
setAttr n (Attr k v) = setAttribute k v n

setAttributes :: ∀ eff. Element → CatList Attr → Eff (dom :: DOM | eff) Unit
setAttributes n = traverse_ (setAttr n)

setEvent :: ∀ eff. Element → EventHandler (E eff) → Eff (dom :: DOM | eff) Unit
setEvent n (EventHandler event handler) =
  addEventListener (EventType event) handler false $ elementToEventTarget n

element :: ∀ eff. NS → String → CatList Attr → CatList (EventHandler (E eff)) → Eff (dom :: DOM | eff) Element
element ns name attrs events = do
  el ← makeElement' ns name
  setAttributes el attrs
  traverse_ (setEvent el) events
  pure el

makeElement' :: ∀ eff. NS → String → Eff (dom :: DOM | eff) Element
makeElement' HTMLns = makeElement
makeElement' SVGns  = makeElementNS "http://www.w3.org/2000/svg"

-- | Render some Smolder markup into a target DOM element.
-- |
-- | Please note that this only appends the Smolder markup as new
-- | child nodes; it does not overwrite the target's current children.
render :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
render target = foldFree (renderNode target)

renderNode :: ∀ foo eff. Element → MarkupM (E eff) foo → Eff (dom :: DOM | eff) foo
renderNode p (Element ns name children attrs events rest) = do
  el ← element ns name attrs events
  render el children
  _ ← appendChild (elementToNode el) (elementToNode p)
  pure rest
renderNode p (Content text rest) = do
  textNode ← makeText text
  _ ← appendChild (textToNode textNode) (elementToNode p)
  pure rest
renderNode p (Empty rest) = pure rest



asAList :: ∀ eff. NodeList → Eff (dom :: DOM | eff) (AList Node)
asAList = nodeListToArray >>> map \a → (AList {index: 0, array: a})

childrenOf :: ∀ eff. Node → Eff (dom :: DOM | eff) (AList Node)
childrenOf parent = childNodes parent >>= asAList

removeNodes :: ∀ eff. Node → AList Node → Eff (dom :: DOM | eff) Unit
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
patch :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
patch parent markup = do
  let node = elementToNode parent
  children ← childrenOf node
  childRef ← unsafeRunRef $ newRef children
  foldFree (walk node childRef) markup
  remainder ← unsafeRunRef $ readRef childRef
  removeNodes node remainder

pop :: ∀ eff. Ref (AList Node) → Eff (dom :: DOM | eff) (Maybe Node)
pop ref = unsafeRunRef $ do
  modifyRef' ref pop'
    where
      pop' l = case uncons l of
        Just (Tuple car cdr) → {state: cdr, value: Just car}
        Nothing → {state: l, value: Nothing}

walk :: ∀ eff. Node → Ref (AList Node) → MarkupM (E eff) ~> Eff (dom :: DOM | eff)
walk parent ref (Empty rest) = do
  -- this node has no children, so remove them if they exist
  children ← unsafeRunRef $ readRef ref
  removeNodes parent children
  pure rest

walk parent ref (Content text rest) = pop ref >>= \node' → case node' of
  Nothing → do
    -- add a text node past end of existing children
    textNode ← makeText text
    _ ← appendChild (textToNode textNode) parent
    pure rest
  Just node → do
    -- patch a text node
    let t = unsafePartial $ nodeType node
    if t == TextNode then setNodeValue text node else do
      textNode ← makeText text
      void $ replaceChild (textToNode textNode) node parent
    pure rest

walk parent ref (Element ns name children attrs events rest) = pop ref >>= \node' → case node' of
  Nothing → do
    -- add element past end of existing children
    el ← element ns name attrs events
    patch el children
    _ ← appendChild (elementToNode el) parent
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
        el ← element ns name attrs events
        patch el children
        void $ replaceChild (elementToNode el) node parent
    pure rest

patchAttrs :: ∀ eff. Element → CatList Attr → Eff (dom :: DOM | eff) Unit
patchAttrs node attrs = patchAttributes node (fromFoldable (map toTuple attrs))
  where toTuple (Attr key value) = Tuple key value

patchEvents :: ∀ eff. Element → CatList (EventHandler (E eff)) → Eff (dom :: DOM | eff) Unit
patchEvents node events = patchEventListeners node (fromFoldable (map toTuple events))
  where toTuple (EventHandler event listener) = Tuple event listener
