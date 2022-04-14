{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module RenderingSpec where

import Prelude hiding (div)
import Data.Aeson.TH
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (classes, once)

import Component
import Events
import Rendering

data SingleConstructor = SingleConstructor

$(deriveJSON (defaultOptions{tagSingleConstructors=True}) ''SingleConstructor)

data TestPurviewT a m = Purview a m

testHandler :: (String -> Purview String IO) -> Purview a IO
testHandler = effectHandler ("" :: String) reducer
  where
    reducer :: String -> String -> IO (String, [DirectedEvent String String])
    reducer action state = pure ("", [])

testOnce = once (\send -> send "")

sizedArbExpr :: Int -> Gen (Purview String IO)
sizedArbExpr 0 = do pure $ text "always present"
sizedArbExpr n = do
  es <- vectorOf 2 (sizedArbExpr (n-1))
  elements
    [ div es
    , style "" $ div es
    , testHandler (const $ div es)
    , testOnce (div es)
    ]

instance Arbitrary (Purview String IO) where
  arbitrary = resize 3 $ sized sizedArbExpr

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
            Attribute (On "click" (1 :: Integer))
            $ Html "div" [Text "hello world"]

      render element `shouldBe`
        "<div action=1>hello world</div>"

    it "can add an id" $ do
      let element = identifier "hello" $ div [text "it's a hello div"]
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
            $ identifier "hello"
            $ div [text "it's a hello div"]
      render element `shouldBe` "<div id=\"hello\" class=\"class1 class2 class3\">it's a hello div</div>"

    it "can render a form" $ do
      let
        named = Attribute . Generic "name"
        input = Html "input"
        form = Html "form"
        component = onSubmit ("initialValue" :: String) $ form [ named "name" $ input [] ]

      render component
        `shouldBe`
        "<form action=\"initialValue\"><input name=\"name\"></input></form>"

    it "can render a typed action" $ do
      let element = onClick SingleConstructor $ div [ text "click" ]

      render element
        `shouldBe`
        "<div action=\"SingleConstructor\">click</div>"

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



main :: IO ()
main = hspec spec
