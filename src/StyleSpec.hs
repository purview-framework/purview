{-# LANGUAGE QuasiQuotes #-}

-- |

module StyleSpec where

import Test.Hspec

import Component (Attributes (Style))
import Style (style)

spec :: SpecWith ()
spec = parallel $ do
  describe "the style quasiquoter" $ do
    it "produces a non-empty style" $ do
      let Style (hash, style') = [style|blue;|]

      hash `shouldSatisfy` (not . null)
      style' `shouldBe` "blue;"

    it "produces the same hash for the same style" $ do
      let
        styleA = [style|blue;|]
        styleB = [style|blue;|]

      styleA `shouldBe` styleB

    it "produces different hashes for different styles" $ do
      let
        Style (hashA, _) = [style|blue;|]
        Style (hashB, _) = [style|red;|]

      hashA `shouldNotBe` hashB
