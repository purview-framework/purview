{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ComponentSpec where

import Test.Hspec
import Data.Aeson
import Component

spec = parallel $ do

  describe "render" $ do

    it "can create a div" $ do
      let element = Html "div" [Text "hello world"]

      render [] element `shouldBe` "<div>hello world</div>"

    it "can add an onclick" $ do
      let element =
            Attribute (OnClick (1 :: Integer))
            $ Html "div" [Text "hello world"]

      render [] element `shouldBe`
        "<div bridge-click=1>hello world</div>"

  describe "apply" $ do
    it "can change state" $ do
      let
        actionHandler :: String -> Int -> Int
        actionHandler "up" state = 1

        handler =
          MessageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render [] handler
        `shouldBe`
        "0"

--      render [] (apply (String "up") handler)
--        `shouldBe`
--        "1"

main = hspec spec
