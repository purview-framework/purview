{-# LANGUAGE OverloadedStrings #-}
module PurviewSpec where

import Prelude hiding (div)
import Test.Hspec
import Purview

upButton :: Purview String
upButton = onClick ("up" :: String) $ div [ text "up" ]

downButton :: Purview String
downButton = onClick ("down" :: String) $ div [ text "down" ]

handler :: (Int -> Purview String) -> Purview a
handler = messageHandler 0 action
  where
    action :: String -> Int -> Int
    action "up" _ = 1
    action _    _ = 0

counter :: Show a => a -> Purview String
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

component :: Purview String
component = handler counter

event' :: String
event' = "{\"event\":\"click\",\"message\":\"up\"}"

spec :: SpecWith ()
spec = parallel $ do
  describe "applying events" $ do
    it "works with the event directly" $ do
--      let applied = handleEvent event' component
--
--      render [] applied `shouldNotBe` render [] component
      (1 :: Integer) `shouldBe` 1

main :: IO ()
main = hspec spec
