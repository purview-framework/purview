module PrepareTreeSpec where

import Prelude hiding (div)
import Test.Hspec
import Test.QuickCheck
import Data.Time

import TreeGenerator
import Events
import Component
import PrepareTree

spec :: SpecWith ()
spec = parallel $ do

  describe "prepareTree" $ do

    it "works across a variety of trees" $ do
      property $ \x -> show (fst (prepareTree (x :: Purview String IO))) `shouldContain` "always present"

    it "sets hasRun to True" $ do
      let
        display time = div
          [ text (show time)
          , onClick ("setTime" :: String) $ div [ text "check time" ]
          ]

        startClock cont state = Once (\send -> send ("setTime" :: String)) False (cont state)

        timeHandler = EffectHandler Nothing Nothing Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state = pure (state, [])

        component = timeHandler (startClock display)

      show (fst (prepareTree component))
        `shouldBe`
        "EffectHandler Just [] Just [] Once True div [  \"Nothing\" Attr div [  \"check time\" ]  ] "

      length (snd (prepareTree component))
        `shouldBe`
        1

    it "stops collecting the action if it has already run" $ do
      let
        display time = div
          [ text (show time)
          , onClick ("setTime" :: String) $ div [ text "check time" ]
          ]

        startClock cont state = Once (\send -> send ("setTime" :: String)) False (cont state)

        timeHandler = EffectHandler Nothing Nothing Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state = pure (state, [])

        component = timeHandler (startClock display)

      let
        run1 = prepareTree component
        run2 = prepareTree (fst run1)
        run3 = prepareTree (fst run2)

      length (snd run1) `shouldBe` 1
      length (snd run2) `shouldBe` 0
      length (snd run3) `shouldBe` 0  -- for a bug where it was resetting run

    it "assigns a location to handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state = pure (state, [])

        component = timeHandler (const (Text ""))

      component `shouldBe` Hide (EffectHandler Nothing Nothing Nothing handle (const (Text "")))

      let
        graphWithLocation = fst (prepareTree component)

      graphWithLocation `shouldBe` Hide (EffectHandler (Just []) (Just []) Nothing handle (const (Text "")))

    it "assigns a different location to child handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state = pure (state, [])

        component = div
          [ timeHandler (const (Text ""))
          , timeHandler (const (Text ""))
          ]

        graphWithLocation = fst (prepareTree component)

      show graphWithLocation
        `shouldBe`
        "div [  Hide EffectHandler Just [] Just [0] \"\" Hide EffectHandler Just [] Just [1] \"\" ] "

    it "assigns a different location to nested handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state = pure (state, [])

        component =
          timeHandler (const (timeHandler (const (Text ""))))


        graphWithLocation = fst (prepareTree component)

      show graphWithLocation `shouldBe` "Hide EffectHandler Just [] Just [] Hide EffectHandler Just [] Just [0] \"\""


main :: IO ()
main = hspec spec
