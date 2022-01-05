module ComponentSpec where

import Test.Hspec

import Component

spec = parallel $ do

  describe "render" $ do

    it "can create a div" $ do
      let element = Html "div" [Text "hello world"]

      render [] element `shouldBe` "<div>hello world</div>"

    it "can add an onclick" $ do
      let element =
            Attribute (OnClick 1)
            $ Html "div" [Text "hello world"]

      render [] element `shouldBe`
        "<div bridge-click=\"click\">hello world</div>"

  describe "apply" $ do
    it "can change state" $ do
      let handler =
            MessageHandler (0 :: Int)
              (\"up" -> 1)
              (Text . show)

      render [] handler
        `shouldBe`
        "0"

      render [] (apply "up" handler)
        `shouldBe`
        "1"

main = hspec spec
