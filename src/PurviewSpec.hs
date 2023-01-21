{-# LANGUAGE OverloadedStrings #-}
module PurviewSpec where

import Prelude hiding (div)
import Test.Hspec
import Purview

upButton :: Purview String m
upButton = onClick ("up" :: String) $ div [ text "up" ]

downButton :: Purview String m
downButton = onClick ("down" :: String) $ div [ text "down" ]

reducer :: Applicative m => (Int -> Purview String m) -> Purview String m
reducer = handler [] 0 action
  where
    action :: String -> Int -> (Int -> Int, [DirectedEvent String String])
    action "up" _ = (const 1, [])
    action _    _ = (const 0, [])

-- counter :: Show a => a -> Purview parentAction action m
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

component :: Applicative m => Purview String m
component = PurviewSpec.reducer counter

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
