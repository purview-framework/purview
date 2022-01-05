{-# LANGUAGE OverloadedStrings #-}
module LibSpec where

import Prelude hiding (div)
import Test.Hspec
import Lib

upButton = onClick ("" :: String) $ div [ text "up" ]
downButton = onClick ("" :: String) $ div [ text "down" ]

handler = MessageHandler 0 action
  where
    action :: String -> Int
    action "up"    = 1
    action "down"  = -1
    action "click" = -2

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

component = handler counter

event' = "{\"event\":\"click\",\"message\":\"click\"}"

spec = parallel $ do
  describe "applying events" $ do
    it "works with the event directly" $ do
      let applied = handleEvent event' component

      render [] applied `shouldNotBe` render [] component

main = hspec spec
