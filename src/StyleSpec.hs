{-# LANGUAGE QuasiQuotes #-}

-- |

module StyleSpec where

import Prelude hiding (div)
import Test.Hspec

import Component
import Style (style)

spec :: SpecWith ()
spec = parallel $ do
  describe "the style quasiquoter" $ do
    let component = div []

    it "produces a non-empty style" $ do
      let styled = [style|blue;|] component

      show styled `shouldBe` "Attr Style (\"p210629223392\",\"blue;\") div [  ] "

    it "produces the same hash for the same style" $ do
      let
        styleA = [style|blue;|] component
        styleB = [style|blue;|] component

      styleA `shouldBe` styleB

    it "produces different hashes for different styles" $ do
      let
        styleA = [style|blue;|] component
        styleB = [style|red;|] component

      styleA `shouldNotBe` styleB

main :: IO ()
main = hspec spec
