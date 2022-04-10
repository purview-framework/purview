module ComponentSpec where

import Test.Hspec

import Component

spec :: SpecWith ()
spec = parallel $ do

  describe "placeholder" $ do

    it "holds a place" $ do
      1 `shouldBe` 1


main :: IO ()
main = hspec spec
