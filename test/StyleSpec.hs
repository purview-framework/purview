{-# LANGUAGE QuasiQuotes #-}

-- |

module StyleSpec where

import Prelude hiding (div)
import Test.Hspec

import Component
import ComponentHelpers
import Style (style, handleCSS, parseLine, parseCSS, Brace (..))

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

    describe "handleCSS" $ do
      it "turns nested CSS into pairs of class and rules" $ do
        let
          css = handleCSS "color: red; div { color: blue; width: 15%; } width: 15%;"

        css `shouldBe` [("","color: red;width: 15%;"),("div ","color: blue;width: 15%;")]

    describe "parseLine" $ do
      it "takes a line (simple)" $ do
        let result = parseLine "\n color: blue; color: red;"
        result `shouldBe` (None, "color: blue;", " color: red;")

      it "says when the line is a new level" $ do
        let result = parseLine "div {}"
        result `shouldBe` (Open, "div {", "}")

    describe "parseCSS" $ do
      it "works for an empty CSS" $ do
        let result = parseCSS [] ""
        result `shouldBe` []

      it "works with some newlines" $ do
        let result = parseCSS [] "\n\n"
        result `shouldBe` []

      it "works with a single rule" $ do
        let result = parseCSS [] "color: blue;"
        result `shouldBe` [("", "color: blue;")]

      it "works with multiple rules on a single level" $ do
        let result = parseCSS [] "color: blue; width: 15px;"
        result `shouldBe` [("", "color: blue;"), ("", "width: 15px;")]

      it "works with multiple rules with returns" $ do
        let result = parseCSS [] "color: blue;\n width: 15px;"
        result `shouldBe` [("", "color: blue;"), ("", "width: 15px;")]

      it "works with a nested rule" $ do
        let result = parseCSS [] "div { color: red; }"
        result `shouldBe` [("div ", "color: red;")]

      it "works with a nested tag and multiple rules" $ do
        let result = parseCSS [] "div { color: red;\n width: 15px; }"
        result `shouldBe` [("div ", "color: red;"), ("div ", "width: 15px;")]

      it "works with a top level rule, then a nested rule, then a top level rule" $ do
        let result = parseCSS [] "color: red;\n div { color: blue; }width: 15px;"
        result `shouldBe` [("", "color: red;"), ("div ", "color: blue;"), ("", "width: 15px;")]

      it "works with a doubly nested rule" $ do
        let result = parseCSS [] "ul { width: 150px;\nli { padding: 15px; } }"
        result `shouldBe` [("ul ","width: 150px;"),("ul li ","padding: 15px;")]


main :: IO ()
main = hspec spec
