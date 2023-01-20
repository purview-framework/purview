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
import EventHandling
import Events
import PrepareTree
import Rendering

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
--  describe "apply" $ do
--
--    it "changes state" $ do
--      let
--        actionHandler :: String -> Int -> (Int -> Int, [DirectedEvent () String])
--        actionHandler "up" _ = (const 1, [])
--        actionHandler _    _ = (const 0, [])
--
--        reducer :: Purview () IO
--        reducer =
--          handler (0 :: Int)
--            actionHandler
--            (Text . show)
--
--      render reducer
--        `shouldBe`
--        "<div handler=\"null\">0</div>"
--
--      chan <- newTChanIO
--
--      let event' = Event { event="click", message="up", location=Nothing }
--
--      appliedHandler <- apply chan event' reducer
--
--      stateEvent <- atomically $ readTChan chan
--
--      show stateEvent `shouldBe` show (StateChangeEvent (id :: Int -> Int) Nothing)
--
--      afterState <- apply chan stateEvent appliedHandler
--
--      render afterState
--        `shouldBe`
--        "<div handler=\"null\">1</div>"
--
--    it "works for clicks across many different trees" $
--      property $ \x -> do
--        let event = Event { event="click", message="up", location=Nothing }
--        chan <- newTChanIO
--
--        component <- apply chan event (x :: Purview String IO)
--        render component `shouldContain` "always present"
--
--    it "works for setting state across many different trees" $
--      property $ \x -> do
--        let event = Event { event="newState", message="up", location=Just [] }
--        chan <- newTChanIO
--
--        component <- apply chan event (x :: Purview String IO)
--        -- this tests 2 things
--        -- 1. that it fully goes down the tree
--        -- 2. the component remains the same, since the event doesn't
--        --    have a location that matches anything
--        component `shouldBe` x
--
--    it "works with typed messages" $ do
--      let
--        actionHandler :: TestAction -> Int -> (Id Int, [DirectedEvent String TestAction])
--        actionHandler Up   _ = (const 1, [])
--        actionHandler Down _ = (const 0, [])
--
--        reducer =
--          handler (0 :: Int)
--            actionHandler
--            (Text . show)
--
--      render reducer
--        `shouldBe`
--        "<div handler=\"null\">0</div>"
--
--      chan <- newTChanIO
--
--      let event' = Event { event="click", message=toJSON Up, location=Nothing }
--
--      appliedHandler <- apply chan event' reducer
--
--      stateEvent <- atomically $ readTChan chan
--
--      afterState <- apply chan stateEvent appliedHandler
--
--      render afterState
--        `shouldBe`
--        "<div handler=\"null\">1</div>"
--
--    it "works after sending an event that did not match anything" $ do
--      let
--        actionHandler :: TestAction -> Int -> (Id Int, [DirectedEvent String TestAction])
--        actionHandler Up   _ = (const 1, [])
--        actionHandler Down _ = (const 0, [])
--
--        reducer =
--          handler (0 :: Int)
--            actionHandler
--            (Text . show)
--
--      chan <- newTChanIO
--
--      let event0 = Event { event="init", message="init", location=Nothing }
--
--      appliedHandler0 <- apply chan event0 reducer
--      render appliedHandler0
--        `shouldBe`
--        "<div handler=\"null\">0</div>"
--
--      let event1 = Event { event="init", message=toJSON Up, location=Nothing }
--
--      appliedHandler1 <- apply chan event1 appliedHandler0
--
--      stateEvent <- atomically $ readTChan chan
--      appliedHandler2 <- apply chan stateEvent appliedHandler1
--
--      render appliedHandler2
--        `shouldBe`
--        "<div handler=\"null\">1</div>"
--
--    it "works with a nested attribute" $ do
--      let
--        childHandler :: TestAction -> Int -> (Id Int, [DirectedEvent String TestAction])
--        childHandler Up   _ = (const 1, [Parent "hello"])
--        childHandler Down _ = (const 0, [])
--
--        parentHandler :: String -> String -> (Id String, [DirectedEvent String String])
--        parentHandler "hello" _ = (const "bye", [])
--        parentHandler "bye" _ = (const "hello", [])
--        parentHandler str _ = (const str, [])
--
--        styledContainer = style "font-size: 10px;" . div
--
--        reducer =
--          handler ("" :: String) parentHandler
--            $ \message ->
--                styledContainer
--                [ text message
--                , handler (0 :: Int)
--                    childHandler
--                    (text . show)
--                ]
--
--        component = reducer
--
--      chan <- newTChanIO
--
--      let
--        locatedGraph = fst $ prepareTree component
--        event1 = Event { event="click", message=toJSON Up, location=Just [1, 0] }
--
--      afterEvent1 <- apply chan event1 locatedGraph
--
--      receivedEvent1 <- atomically $ readTChan chan
--      show receivedEvent1 `shouldBe` show (StateChangeEvent (id :: Int -> Int) (Just [1, 0]))
--
--      receivedEvent2 <- atomically $ readTChan chan
--      receivedEvent2 `shouldBe` Event {event = "internal", message = String "hello", location = Just []}
--
--    describe "sending events" $ do
--
--      it "can send an event to a parent" $ do
--        let
--          childHandler :: TestAction -> Int -> (Id Int, [DirectedEvent String TestAction])
--          childHandler Up   _ = (const 1, [Parent "hello"])
--          childHandler Down _ = (const 0, [])
--
--          parentHandler :: String -> String -> (Id String, [DirectedEvent String String])
--          parentHandler "hello" _ = (const "bye", [])
--          parentHandler "bye" _ = (const "hello", [])
--          parentHandler str _ = (const str, [])
--
--          reducer =
--            handler ("" :: String) parentHandler
--              $ \message ->
--                  div
--                  [ text message
--                  , handler (0 :: Int)
--                      childHandler
--                      (text . show)
--                  ]
--
--        chan <- newTChanIO
--
--        let locatedGraph = fst $ prepareTree reducer
--
--        render locatedGraph `shouldBe` "<div handler=\"[]\"><div><div handler=\"[1,0]\">0</div></div></div>"
--
--        let event1 = Event { event="click", message=toJSON Up, location=Just [1, 0] }
--
--        afterEvent1 <- apply chan event1 locatedGraph
--
--        receivedEvent1 <- atomically $ readTChan chan
--        show receivedEvent1 `shouldBe` show (StateChangeEvent (id :: Int -> Int) (Just [1, 0]))
--
--        receivedEvent2 <- atomically $ readTChan chan
--        -- correctly targeted to the parent
--        receivedEvent2 `shouldBe` Event {event = "internal", message = String "hello", location = Just []}
--
--
--      it "can send an event to self" $ do
--        let
--          childHandler :: TestAction -> Int -> (Int -> Int, [DirectedEvent String TestAction])
--          childHandler Up   _ = (const 1, [Self Down])
--          childHandler Down _ = (const 0, [])
--
--          parentHandler :: String -> String -> (String -> String, [DirectedEvent String String])
--          parentHandler "hello" _ = (const "bye", [])
--          parentHandler "bye" _ = (const "hello", [])
--          parentHandler str _ = (const str, [])
--
--          reducer =
--            handler ("" :: String) parentHandler
--              $ \message ->
--                  div
--                  [ text message
--                  , handler (0 :: Int)
--                      childHandler
--                      (text . show)
--                  ]
--
--        chan <- newTChanIO
--
--        let locatedGraph = fst $ prepareTree reducer
--
--        render locatedGraph `shouldBe` "<div handler=\"[]\"><div><div handler=\"[1,0]\">0</div></div></div>"
--
--        let event1 = Event { event="click", message=toJSON Up, location=Just [1, 0] }
--
--        afterEvent1 <- apply chan event1 locatedGraph
--
--        receivedEvent1 <- atomically $ readTChan chan
--        show receivedEvent1 `shouldBe` show (StateChangeEvent (id :: Int -> Int) (Just [1, 0]))
--
--        receivedEvent2 <- atomically $ readTChan chan
--        -- correctly targeted to self
--        receivedEvent2 `shouldBe` Event {event = "internal", message = String "Down", location = Just [1,0]}

  describe "findEvent" $ do
    it "works" $ do
      let
        clickHandler :: (Int -> Purview String IO) -> Purview () IO
        clickHandler = handler [] (0 :: Int) reducer

        reducer :: String -> Int -> (Int -> Int, [DirectedEvent () String])
        reducer "up" st = (const 0, [])
        reducer "down" st = (const 1, [])

        tree = clickHandler $ const $ div [ onClick "up" $ div [ text "up" ] ]
        (_, treeWithLocations) = prepareTree tree

        -- EffectHandler Just [] Just [] "0" div [  Attr On "click" Just [0,0] div [  "up" ]  ]
        event' = Event { kind="click", childLocation=Just [0, 0], location=Just [] }

      findEvent event' treeWithLocations
        `shouldBe`
        (Just $ AnyEvent ("up" :: String) (Just [0, 0]) (Just []))

main :: IO ()
main = hspec spec
