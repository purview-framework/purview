module PrepareTreeSpec where

import Prelude hiding (div)
import Test.Hspec
import Test.QuickCheck

import TreeGenerator ()
import Events
import Component
import ComponentHelpers
import PrepareTree
import CollectInitials (collectInitials)
import CleanTree (cleanTree)

type UTCTime = Integer

getCurrentTime :: IO Integer
getCurrentTime = pure 10

spec :: SpecWith ()
spec = parallel $ do

  describe "prepareTree" $ do

    it "works across a variety of trees" $ do
      property $ \x -> show (prepareTree (x :: Purview String IO))
        `shouldContain` "always present"

    it "assigns an identifier to On actions" $ do
      let target = div
            [ onClick "setTime" $ div []
            , onClick "clearTime" $ div []
            ]
          fixedTree = prepareTree target

      fixedTree
        `shouldBe`
        Html "div"
          [ Attribute (On "click" (Just [0]) (const "setTime") ) $ Html "div" []
          , Attribute (On "click" (Just [1]) (const "clearTime") ) $ Html "div" []
          ]

    -- TODO: Nested On actions

    describe "collecting initial events" $ do

      it "works for handlers" $ do
        let
          handler' :: (String -> Purview String IO) -> Purview () IO
          handler' = handler [Self "up"] "" handle

          handle "up" state = (id, [])

          preparedTree = prepareTree (handler' (const $ div []))
          (initialActions, _) = collectInitials preparedTree

        initialActions `shouldBe` [InternalEvent "up" Nothing (Just [])]

        -- the next round there should be no initial actions
        let
          (initialActions', _) = collectInitials $ cleanTree [] (prepareTree $ handler' (const $ div []))

        initialActions' `shouldBe` []

      it "works for effectHandler" $ do
        let
          handler' = effectHandler' [Self "up"] "" handle

          handle "up" state = pure (state, []) :: IO (String, [DirectedEvent () String])

          preparedTree = prepareTree (handler' (const $ div []))
          (initialActions, _) =  collectInitials preparedTree

        initialActions `shouldBe` [InternalEvent "up" Nothing (Just [])]

        -- the next round there should be no initial actions
        let
          (initialActions', _) = collectInitials $ cleanTree [] (prepareTree $ handler' (const $ div []))

        initialActions' `shouldBe` []

      it "works for nested handlers" $ do
        let
          parentHandler = handler' [] "" handle
          childHandler = handler' [Self "to child", Parent "to parent"] "" handle

          handle "" state = (state, [])

          component :: Purview () IO
          component = parentHandler $ \_ -> childHandler $ \_ -> div []

          preparedTree = prepareTree component
          (initialActions, _) = collectInitials preparedTree

        initialActions
          `shouldBe` [ InternalEvent "to child" Nothing (Just [0])
                     , InternalEvent "to parent" Nothing (Just [])
                     ]

    it "assigns a location to handlers" $ do
      let
        timeHandler = effectHandler [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime -> Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (const $ Just time, [])
        handle _         state =
          pure (const state, [])

        component = timeHandler (const (Text ""))

      component `shouldBe` (EffectHandler Nothing Nothing [] Nothing handle (const (Text "")))

      let
        graphWithLocation = prepareTree component

      graphWithLocation `shouldBe` (EffectHandler (Just []) (Just []) [] Nothing handle (const (Text "")))

    it "assigns a different location to child handlers" $ do
      let
        timeHandler = effectHandler' [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state =
          pure (state, [])

        component = div
          [ timeHandler (const (Text ""))
          , timeHandler (const (Text ""))
          ]

        graphWithLocation = prepareTree component

      show graphWithLocation
        `shouldBe`
        "div [  EffectHandler Just [] Just [0] Nothing \"\" EffectHandler Just [] Just [1] Nothing \"\" ] "

    it "assigns a different location to nested handlers" $ do
      let
        timeHandler = effectHandler' [] Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime, [DirectedEvent String String])
        handle "setTime" _     = do
          time <- getCurrentTime
          pure (Just time, [])
        handle _         state =
          pure (state, [])

        component =
          timeHandler (const (timeHandler (const (Text ""))))


        graphWithLocation = prepareTree component

      show graphWithLocation `shouldBe` "EffectHandler Just [] Just [] Nothing EffectHandler Just [] Just [0] Nothing \"\""

    it "picks up css" $ do
      let
        component :: Purview () m
        component = (Attribute $ Style ("123", "color: blue;")) $ div []

        (_, css) = collectInitials component :: ([Event], [(Hash, String)])

      css `shouldBe` [("123", "color: blue;")]



main :: IO ()
main = hspec spec
