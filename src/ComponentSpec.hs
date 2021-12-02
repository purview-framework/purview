-- |

module ComponentSpec where

import Component
import Test.Hspec

spec = parallel $ do
  describe "test" $ do
    it "don't" $ do
      1 `shouldBe` 2

main = hspec spec
