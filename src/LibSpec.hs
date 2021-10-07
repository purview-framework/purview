module LibSpec where

import Test.Hspec

spec = parallel $ do
  describe "test" $ do
    it "does" $ do
      1 `shouldBe` 1
