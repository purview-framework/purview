module DiffingSpec where

import Prelude hiding (div)
import Test.Hspec

import Diffing
import Purview


spec :: SpecWith ()
spec = parallel $ do

  describe "diff" $ do

    it "creates an update for differing text" $ do
      let
        oldTree = div [ text "night" ]
        newTree = div [ text "morning" ]

      diff [] oldTree newTree `shouldBe` [Update [0] (text "morning")]

    it "can do a nested update" $ do
      let
        oldTree = div [ div [ text "night" ] ]
        newTree = div [ div [ text "morning" ] ]

      diff [] oldTree newTree `shouldBe` [Update [0, 0] (text "morning")]

    it "says to update the whole underlying tree on added div" $ do
      let
        oldTree = div [ text "night" ]
        newTree = div [ div [ text "morning" ] ]

      diff [] oldTree newTree `shouldBe` [Update [0] (div [ text "morning" ])]

    it "doesn't diff handler children if the state is the same" $ do
      let
        mkHandler = messageHandler "initial state" (\action state -> state <> action)
        oldTree = div [ mkHandler (const (text "the original")) ]
        newTree = div [ mkHandler (const (text "this is different")) ]

      diff [] oldTree newTree `shouldBe` []

    it "diffs handler children if the state is different" $ do
      let
        handler1 = messageHandler "initial state" (\action state -> state <> action)
        handler2 = messageHandler "different state" (\action state -> state <> action)
        oldTree = div [ handler1 (const (text "the original")) ]
        newTree = div [ handler2 (const (text "this is different")) ]

      diff [] oldTree newTree `shouldBe` [Update [0] (handler2 (const (text "this is different")))]


main :: IO ()
main = hspec spec
