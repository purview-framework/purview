{-# LANGUAGE NoMonomorphismRestriction #-}
module PrepareTreeSpec where

import Prelude hiding (div)
import Test.Hspec
import Test.QuickCheck
import Data.Time

import TreeGenerator ()
import Events
import Component
import PrepareTree

spec :: SpecWith ()
spec = parallel $ do

  describe "prepareTree" $ do

    it "works across a variety of trees" $ do
      property $ \x -> show (snd (prepareTree (x :: Purview String IO))) `shouldContain` "always present"

    it "assigns an identifier to On actions" $ do
      let target = div
            [ onClick "setTime" $ div []
            , onClick "clearTime" $ div []
            ]
          fixedTree = prepareTree target

      snd fixedTree
        `shouldBe`
        Html "div"
          [ Attribute (On "click" (Just [0]) "setTime" ) $ Html "div" []
          , Attribute (On "click" (Just [1]) "clearTime" ) $ Html "div" []
          ]

    -- TODO: Nested On actions

    describe "collecting initial events" $ do

      it "works for handlers" $ do
        let
          handler' = handler [Self "up"] "" handle

          handle "up" _ = (id, [])

          (initialActions, component) = prepareTree (handler' (const $ div []))

        initialActions `shouldBe` [AnyEvent "up" Nothing (Just [])]

        -- the next round there should be no initial actions
        let
          (initialActions', component') = prepareTree component

        initialActions' `shouldBe` []

      it "works for effectHandler" $ do
        let
          handler' = effectHandler [Self "up"] "" handle

          handle "up" _ = pure (id, []) :: IO (a -> a, [DirectedEvent () String])

          (initialActions, component) = prepareTree (handler' (const $ div []))

        initialActions `shouldBe` [AnyEvent "up" Nothing (Just [])]

        -- the next round there should be no initial actions
        let
          (initialActions', component') = prepareTree component

        initialActions' `shouldBe` []

      it "works for nested handlers" $ do
        let
          handler' = Handler

        1 `shouldBe` 1

    it "assigns a location to handlers" $ do
      let
        timeHandler = effectHandler [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component = timeHandler (const (Text ""))

      component `shouldBe` (EffectHandler Nothing Nothing [] Nothing handle (const (Text "")))

      let
        graphWithLocation = snd (prepareTree component)

      graphWithLocation `shouldBe` (EffectHandler (Just []) (Just []) [] Nothing handle (const (Text "")))

    it "assigns a different location to child handlers" $ do
      let
        timeHandler = effectHandler [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component = div
          [ timeHandler (const (Text ""))
          , timeHandler (const (Text ""))
          ]

        graphWithLocation = snd (prepareTree component)

      show graphWithLocation
        `shouldBe`
        "div [  EffectHandler Just [] Just [0] Nothing \"\" EffectHandler Just [] Just [1] Nothing \"\" ] "

    it "assigns a different location to nested handlers" $ do
      let
        timeHandler = effectHandler [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component =
          timeHandler (const (timeHandler (const (Text ""))))


        graphWithLocation = snd (prepareTree component)

      show graphWithLocation `shouldBe` "EffectHandler Just [] Just [] Nothing EffectHandler Just [] Just [0] Nothing \"\""


main :: IO ()
main = hspec spec
