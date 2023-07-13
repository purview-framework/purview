module DiffingSpec where

import Prelude hiding (div)
import Test.Hspec

import PrepareTree
import Purview
import Component
import Diffing

type DefaultAction = DirectedEvent String String

spec :: SpecWith ()
spec = parallel $ do

  describe "diff" $ do

    it "creates an update for differing text" $ do
      let
        oldTree = div [ text "night" ]
        newTree = div [ text "morning" ]

      diff Nothing [] oldTree newTree `shouldBe` [Update [0] (text "morning")]

    it "can do a nested update" $ do
      let
        oldTree = div [ div [ text "night" ] ]
        newTree = div [ div [ text "morning" ] ]

      diff Nothing [] oldTree newTree `shouldBe` [Update [0, 0] (text "morning")]

    it "says to update the whole underlying tree on added div" $ do
      let
        oldTree = div [ text "night" ]
        newTree = div [ div [ text "morning" ] ]

      diff Nothing [] oldTree newTree `shouldBe` [Update [0] (div [ text "morning" ])]

    describe "message handlers" $ do

      it "diffs handler children if the state is different" $ do
        let
          handler1 :: (String -> Purview String IO) -> Purview String IO
          handler1 = handler [] "initial state" (\action state -> (const $ state <> action, [] :: [DefaultAction]))
          handler2 = handler [] "different state" (\action state -> (const $ state <> action, [] :: [DefaultAction]))
          oldTree = div [ handler1 (const (text "the original")) ]
          newTree = div [ handler2 (const (text "this is different")) ]

        diff Nothing [] oldTree newTree `shouldBe`
          [ Update [0] (handler2 (const (text "this is different"))) ]

    describe "effect handlers" $ do

      it "diffs handler children if the state is different" $ do
        let
          handler1 :: (String -> Purview String IO) -> Purview String IO
          handler1 = effectHandler [] "initial state" (\action state -> pure $ (const $ state <> action, ([] :: [DirectedEvent String String])))
          handler2 = effectHandler [] "different state" (\action state -> pure $ (const $ state <> action, ([] :: [DirectedEvent String String])))
          oldTree = div [ handler1 (const (text "the original")) ]
          newTree = div [ handler2 (const (text "this is different")) ]

        diff Nothing [] oldTree newTree `shouldBe`
          [ Update [0] (handler2 (const (text "this is different")))
          , Update [0, 0] (text "this is different")
          ]

      it "continues going down the tree even if the state is the same at the top" $ do
        let
          handler1 :: (String -> Purview String IO) -> Purview String IO
          handler1 = effectHandler [] "initial state" (\action state -> pure $ (const $ state <> action, ([] :: [DirectedEvent String String])))
          handler2 = effectHandler [] "different state" (\action state -> pure $ (const $ state <> action, ([] :: [DirectedEvent String String])))
          oldTree = prepareTree $ div [ handler1 . const $ handler1 (const (text "the original")) ]
          newTree = prepareTree $ div [ handler1 . const $ handler2 (const (text "this is different")) ]

        diff (Just [0, 0]) [] oldTree newTree `shouldBe`
          [ Update [0, 0] (EffectHandler
                            (Just [0])
                            (Just [0, 0])
                            []
                            "different state"
                            (\action state -> pure $ (const $ state <> action, ([] :: [DirectedEvent String String])))
                            (const (text "this is different")))
          ]


main :: IO ()
main = hspec spec
