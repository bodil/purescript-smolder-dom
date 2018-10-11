module Test.Main where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Unit (Test, failure, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Text.Smolder.HTML (blockquote, button, div, h1, h2, h3, p)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (Markup, on, text, (!), (#!))
import Text.Smolder.Renderer.DOM (patch, render)
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (click)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

foreign import runWithDom :: ∀ a. Effect a → Effect a
foreign import innerHTML :: Element → String

assertHTML :: String → Element → Test
assertHTML s n = equal s $ innerHTML n

render' :: Markup EventListener → Effect Element
render' m = do
  doc ← window >>= document >>= toDocument >>> pure
  el ← createElement "body" doc
  render el m
  pure el

makeListener :: Aff { listener :: EventListener, wasCalled :: Aff Boolean }
makeListener = liftEffect do
  called ← Ref.new false
  let wasCalled = liftEffect $ Ref.read called
  listener ← eventListener \_ → Ref.write true called
  pure { listener, wasCalled }

main :: Effect Unit
main = runWithDom $ runTest do

  test "renders to the expected DOM nodes" do
    doc ← liftEffect $ render' $ div ! className "lol" ! id "omg" $ do
          p $ text "omg wtf"
          blockquote ! className "big" $ text "bbq"
    assertHTML "<div class=\"lol\" id=\"omg\"><p>omg wtf</p><blockquote class=\"big\">bbq</blockquote></div>" doc

  test "registers event handlers" do
    l ← makeListener
    b' ← liftEffect $ do
      doc ← render' $ button #! (on "click" l.listener) $ text "button"
      querySelector (QuerySelector "button") (Element.toParentNode doc)
    case join $ HTMLElement.fromElement <$> b' of
      Nothing → failure "button wasn't an HTML element"
      Just b → do
        liftEffect $ click b
        l.wasCalled >>= assert "button's event handler wasn't called"

  test "patch an existing node" do
    doc ← liftEffect $ render' $ do
          h1 $ text "header"
          div ! className "lol" $ do
            p $ text "omg hai"
            p $ text "omg lol"
            p $ text "omg wat"
    liftEffect $ patch doc $ do
      h2 $ text "subheader"
      div ! id "wat" $ do
        h3 $ text "subsubheader"
        p $ text "nope"
    assertHTML "<h2>subheader</h2><div id=\"wat\"><h3>subsubheader</h3><p>nope</p></div>" doc
