{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module EventHandlingSpec where

import Prelude hiding (div)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import Data.Aeson.TH

import TreeGenerator
import Component
import ComponentHelpers
import EventHandling
import Events
import PrepareTree
import Rendering

import Data.Typeable

type Id a = a -> a

data TestAction = Up | Down

$(deriveJSON defaultOptions ''TestAction)

data SingleConstructor = SingleConstructor

$(deriveJSON (defaultOptions{tagSingleConstructors=True}) ''SingleConstructor)

{-

Just to clean up tests a bit. I dunno if this approach would work to clean
up the main event loop as well, since it calls "runEvent" (re: applying the event)
in a non-forked fashioned.  If you try void . forkIO you end up back in m a -> IO a
hell.

-}
-- apply :: MonadIO m => TChan Event -> Event -> Purview event m -> m (Purview event m)
-- apply eventBus newStateEvent@StateChangeEvent {} component =
--   pure $ applyNewState newStateEvent component
-- apply eventBus fromEvent@Event {event=eventKind} component =
--   case eventKind of
--     "newState" -> pure $ applyNewState fromEvent component
--     _          -> do
--       events <- runEvent fromEvent component
--       liftIO $ mapM_ (atomically . writeTChan eventBus) events
--       pure component

spec :: SpecWith ()
spec = parallel $ do

  describe "applyNewState" $ do
    it "applies new state at the top level" $ do
      let
        reducer "test" st = (1, [])
        reducer _      st = (0, [])

        clickHandler :: (Int -> Purview String IO) -> Purview () IO
        clickHandler = handler' [] 0 reducer

        component = prepareTree $ clickHandler $ \state -> div [ text (show state) ]

        event = StateChangeEvent (\state -> state + 1 :: Int) (Just [])

        appliedClickHandler :: (Int -> Purview String IO) -> Purview () IO
        appliedClickHandler = handler' [] 1 reducer

        applied = prepareTree $ appliedClickHandler $ \state -> div [ text (show state) ]

      -- state should now be 1
      applyNewState event component `shouldBe` applied

    it "applies new state at a lower level" $ do
      let
        reducer "test" st = (1, [])
        reducer _      st = (0, [])

        clickHandler :: (Int -> Purview String IO) -> Purview String IO
        clickHandler = handler' [] 0 reducer

        component = prepareTree $ clickHandler $ \_ -> clickHandler $ \_ -> div []

        event = StateChangeEvent (\state -> state + 1 :: Int) (Just [0])

        appliedClickHandler :: (Int -> Purview String IO) -> Purview String IO
        appliedClickHandler = handler' [] 1 reducer

        applied = prepareTree $ clickHandler $ \_ -> appliedClickHandler $ \_ -> div []

      -- state should now be 1
      applyNewState event component `shouldBe` applied


  describe "runEvent" $ do
    it "applies an event at the top level" $ do
      let
        reducer "test" st = (1, [])
        reducer _      st = (0, [])

        clickHandler :: (Int -> Purview String IO) -> Purview () IO
        clickHandler = handler' [] (0 :: Int) reducer

        component = prepareTree $ clickHandler $ \state -> div [ text (show state) ]

        event = InternalEvent { event = "test" :: String, childId = Nothing, handlerId = Just [] }

      [stateChangeEvent] <- runEvent event component

      case stateChangeEvent of
        StateChangeEvent fn id -> case cast fn of
          Just fn' -> fn' (0 :: Int) `shouldBe` (1 :: Int)
          _        -> fail "state change fn wrong type"
        _ -> fail "didn't return a state change fn"

    it "applies an event to the lower level" $ do
      let
        reducerA "test" st = (1, [])
        reducerA _      st = (0, [])

        clickHandlerA :: (Int -> Purview String IO) -> Purview () IO
        clickHandlerA = handler' [] (0 :: Int) reducerA

        reducerB "test" st = (5, [])
        reducerB _      st = (6, [])

        clickHandlerB :: (Int -> Purview String IO) -> Purview String IO
        clickHandlerB = handler' [] (0 :: Int) reducerB

        component = prepareTree $ clickHandlerA $ \_ -> clickHandlerB $ \_ -> div []

        event = InternalEvent { event = "test" :: String, childId = Nothing, handlerId = Just [0] }

      [stateChangeEvent] <- runEvent event component

      case stateChangeEvent of
        StateChangeEvent fn id -> case cast fn of
          Just fn' -> fn' (0 :: Int) `shouldBe` (5 :: Int)
          _        -> fail "state change fn wrong type"
        _ -> fail "didn't return a state change fn"


  describe "findEvent" $ do
    it "works" $ do
      let
        reducer "test" st = (const 1, [])
        reducer _      st = (const 0, [])

        clickHandler :: (Int -> Purview String IO) -> Purview () IO
        clickHandler = handler [] (0 :: Int) reducer

        tree = clickHandler $ const $ div [ onClick "test" $ div [ text "up" ] ]
        treeWithLocations = prepareTree tree

        -- EffectHandler Just [] Just [] "0" div [  Attr On "click" Just [0,0] div [  "up" ]  ]
        event' = FromFrontendEvent { kind="click", childLocation=Just [0, 0], location=Just [] }

      findEvent event' treeWithLocations
        `shouldBe`
        Just (InternalEvent ("test" :: String) (Just [0, 0]) (Just []))

main :: IO ()
main = hspec spec
