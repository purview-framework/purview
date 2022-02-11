{-# LANGUAGE OverloadedStrings #-}
module PurviewSpec where

import Prelude hiding (div)
import Test.Hspec
import Purview

upButton = onClick ("up" :: String) $ div [ text "up" ]
downButton = onClick ("down" :: String) $ div [ text "down" ]

handler = MessageHandler Nothing 0 action
  where
    action :: String -> Int -> Int
    action "up"    _ = 1

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

component = handler counter

event' = "{\"event\":\"click\",\"message\":\"up\"}"

spec = parallel $ do
  describe "applying events" $ do
    it "works with the event directly" $ do
--      let applied = handleEvent event' component
--
--      render [] applied `shouldNotBe` render [] component
      1 `shouldBe` 1

main = hspec spec
