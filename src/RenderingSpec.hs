module RenderingSpec where

import Prelude hiding (div)
import Data.Aeson.TH
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (classes, once)

import TreeGenerator
import Component
import Events
import Rendering
import Component (onSubmit)

data SingleConstructor = SingleConstructor
  deriving (Show, Eq)


spec :: SpecWith ()
spec = parallel $ do

  describe "render" $ do

    it "can render an assortment of different trees" $
      property $ \x -> render (x :: Purview String IO) `shouldContain` "always present"

    it "can create a div" $ do
      let element = Html "div" [Text "hello world"]

      render element `shouldBe` "<div>hello world</div>"

    it "can add an onclick" $ do
      let element =
            Attribute (On "click" Nothing (\_ -> 1 :: Integer))
            $ Html "div" [Text "hello world"]

      render element `shouldBe`
        "<div bubbling-bound click-location=null>hello world</div>"

    it "can add an id" $ do
      let element = id' "hello" $ div [text "it's a hello div"]
      render element `shouldBe` "<div id=\"hello\">it's a hello div</div>"

    it "can add one class" $ do
      let element =
            classes ["class1"] $ div [text "it's a hello div"]
      render element `shouldBe` "<div class=\"class1\">it's a hello div</div>"

    it "can add classes" $ do
      let element =
            classes ["class1", "class2", "class3"] $ div [text "it's a hello div"]
      render element `shouldBe` "<div class=\"class1 class2 class3\">it's a hello div</div>"

    it "can render classes and ids at the same time" $ do
      let element =
            classes ["class1", "class2", "class3"]
            $ id' "hello"
            $ div [text "it's a hello div"]
      render element `shouldBe` "<div id=\"hello\" class=\"class1 class2 class3\">it's a hello div</div>"

    it "can render a form" $ do
      let
        named = Attribute . Generic "name"
        input = Html "input"
        form = Html "form"
        component = onSubmit (\_ -> "initialValue" :: String) $ form [ named "name" $ input [] ]

      render component
        `shouldBe`
        "<form bubbling-bound submit-location=null><input name=\"name\"></input></form>"

    it "can render a typed action" $ do
      let element = onClick SingleConstructor $ div [ text "click" ]

      render element
        `shouldBe`
        "<div bubbling-bound click-location=null>click</div>"

    it "can render two typed actions of different form" $ do
      let element
            = onSubmit (const SingleConstructor)
            $ onClick SingleConstructor
            $ div [ text "click" ]

      render element
        `shouldBe`
        "<div bubbling-bound click-location=null submit-location=null>click</div>"


    it "can render a style" $ do
      let element = style "color: blue;" $ div [ text "blue" ]

      render element
        `shouldBe`
        "<div style=\"color: blue;\">blue</div>"

    it "can render composed styles" $ do
      let blue = style "color: blue;"
          halfSize = style "width: 50%; height: 50%;"

      render (blue . halfSize $ div [ text "box" ])
        `shouldBe`
        "<div style=\"width: 50%; height: 50%;color: blue;\">box</div>"

    it "can render a receiver" $ do
      let receiver = Receiver (Just []) (Just [0, 1]) "test" (const "")

      render receiver
        `shouldBe`
        "<div handler=\"[0,1]\" parent-handler=\"[]\" receiver-name=\"test\"></div>"


main :: IO ()
main = hspec spec
