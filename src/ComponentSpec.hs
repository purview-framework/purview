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
            Attribute OnClick "something"
            $ Html "div" [Text "hello world"]

      render [] element `shouldBe`
        "<div bridge-click=\"clicked\">hello world</div>"

main = hspec spec
