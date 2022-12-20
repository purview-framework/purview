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

    it "assigns an identifier to On actions" $ do
      let target = div
            [ onClick "setTime" $ div []
            , onClick "clearTime" $ div []
            ]
          (preparedTarget, _) = prepareTree target

      preparedTarget
        `shouldBe`
        Html "div"
          [ Attribute (On "click" Nothing "setTime" ) $ Html "div" []
          , Attribute (On "click" Nothing "clearTime" ) $ Html "div" []
          ]

    it "sets hasRun to True" $ do
      let
        display time = div
          [ text (show time)
          , onClick ("setTime" :: String) $ div [ text "check time" ]
          ]

        startClock cont state = Once (\send -> send ("setTime" :: String)) False (cont state)

        timeHandler = EffectHandler Nothing Nothing Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component = timeHandler (startClock display)

      show (fst (prepareTree component))
        `shouldBe`
        "EffectHandler Just [] Just [] \"null\" Once True div [  \"Nothing\" Attr div [  \"check time\" ]  ] "

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

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

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

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component = timeHandler (const (Text ""))

      component `shouldBe` (EffectHandler Nothing Nothing Nothing handle (const (Text "")))

      let
        graphWithLocation = fst (prepareTree component)

      graphWithLocation `shouldBe` (EffectHandler (Just []) (Just []) Nothing handle (const (Text "")))

    it "assigns a different location to child handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component = div
          [ timeHandler (const (Text ""))
          , timeHandler (const (Text ""))
          ]

        graphWithLocation = fst (prepareTree component)

      show graphWithLocation
        `shouldBe`
        "div [  EffectHandler Just [] Just [0] \"null\" \"\" EffectHandler Just [] Just [1] \"null\" \"\" ] "

    it "assigns a different location to nested handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state = pure (const state, [])

        component =
          timeHandler (const (timeHandler (const (Text ""))))


        graphWithLocation = fst (prepareTree component)

      show graphWithLocation `shouldBe` "EffectHandler Just [] Just [] \"null\" EffectHandler Just [] Just [0] \"null\" \"\""


main :: IO ()
main = hspec spec
