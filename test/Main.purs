module Test.Main where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (click)
import DOM.HTML.Types (htmlDocumentToDocument, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (Element, elementToParentNode)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Test.Unit (Test, failure, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Smolder.HTML (blockquote, button, div, h1, h2, h3, p)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (Markup, on, text, (!), (#!))
import Text.Smolder.Renderer.DOM (patch, render)
import Prelude hiding (div,id)

foreign import runWithDom :: ∀ e a. Eff e a → Eff e a
foreign import innerHTML :: Element → String

assertHTML :: ∀ e. String → Element → Test e
assertHTML s n = equal s $ innerHTML n

render' :: ∀ eff. Markup (EventListener (dom :: DOM | eff)) → Eff (dom :: DOM | eff) Element
render' m = do
  doc ← window >>= document >>= htmlDocumentToDocument >>> pure
  el ← createElement "body" doc
  render el m
  pure el

liftMaybe :: ∀ e a. Eff e (Maybe a) → Aff e a
liftMaybe eff = makeAff \lose win → do
  result ← eff
  case result of
    Nothing → lose (error "nothing")
    Just a → win a

makeListener :: ∀ e. Aff (ref :: REF | e) { listener :: EventListener (ref :: REF | e), wasCalled :: Aff (ref :: REF | e) Boolean }
makeListener = liftEff do
  called ← newRef false
  let listener = eventListener \_ → writeRef called true
      wasCalled = liftEff $ readRef called
  pure { listener, wasCalled }

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, dom :: DOM, ref :: REF | e) Unit
main = runWithDom $ runTest do

  test "renders to the expected DOM nodes" do
    doc ← liftEff $ render' $ div ! className "lol" ! id "omg" $ do
          p $ text "omg wtf"
          blockquote ! className "big" $ text "bbq"
    assertHTML "<div class=\"lol\" id=\"omg\"><p>omg wtf</p><blockquote class=\"big\">bbq</blockquote></div>" doc

  test "registers event handlers" do
    l ← makeListener
    doc ← liftEff $ render' $ button #! (on "click" l.listener) $ text "button"
    b' ← liftMaybe $ querySelector (QuerySelector "button") (elementToParentNode doc)
    case runExcept $ readHTMLElement (toForeign b') of
      Left _ → failure "button wasn't an HTML element"
      Right b → do
        liftEff $ click b
        l.wasCalled >>= assert "button's event handler wasn't called"

  test "patch an existing node" do
    doc ← liftEff $ render' $ do
          h1 $ text "header"
          div ! className "lol" $ do
            p $ text "omg hai"
            p $ text "omg lol"
            p $ text "omg wat"
    liftEff $ patch doc $ do
      h2 $ text "subheader"
      div ! id "wat" $ do
        h3 $ text "subsubheader"
        p $ text "nope"
    assertHTML "<h2>subheader</h2><div id=\"wat\"><h3>subsubheader</h3><p>nope</p></div>" doc
