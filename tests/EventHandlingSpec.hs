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

apply :: MonadIO m => TChan FromEvent -> FromEvent -> Purview a m -> m (Purview a m)
apply eventBus fromEvent@FromEvent {event=eventKind} component =
  case eventKind of
    "newState" -> pure $ applyNewState fromEvent component
    _          -> do
      events <- runEvent fromEvent component
      liftIO $ mapM_ (atomically . writeTChan eventBus) events
      pure component

spec :: SpecWith ()
spec = parallel $ do
  describe "apply" $ do

    it "changes state" $ do
      let
        actionHandler :: String -> Int -> (Int, [DirectedEvent String String])
        actionHandler "up" _ = (1, [])
        actionHandler _    _ = (0, [])

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render handler
        `shouldBe`
        "<div handler=\"null\">0</div>"

      chan <- newTChanIO

      let event' = FromEvent { event="click", message="up", location=Nothing }

      appliedHandler <- apply chan event' handler

      stateEvent <- atomically $ readTChan chan

      stateEvent
        `shouldBe`
        FromEvent { event="newState", message=Number 1, location=Nothing }

      afterState <- apply chan stateEvent appliedHandler

      render afterState
        `shouldBe`
        "<div handler=\"null\">1</div>"

    it "works for clicks across many different trees" $
      property $ \x -> do
        let event = FromEvent { event="click", message="up", location=Nothing }
        chan <- newTChanIO

        component <- apply chan event (x :: Purview String IO)
        render component `shouldContain` "always present"

    it "works for setting state across many different trees" $
      property $ \x -> do
        let event = FromEvent { event="newState", message="up", location=Nothing }
        chan <- newTChanIO

        component <- apply chan event (x :: Purview String IO)
        -- this tests 2 things
        -- 1. that it fully goes down the tree
        -- 2. the component remains the same, since the event doesn't
        --    have a location that matches anything
        component `shouldBe` x

    it "works with typed messages" $ do
      let
        actionHandler :: TestAction -> Int -> (Int, [DirectedEvent String TestAction])
        actionHandler Up   _ = (1, [])
        actionHandler Down _ = (0, [])

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render handler
        `shouldBe`
        "<div handler=\"null\">0</div>"

      chan <- newTChanIO

      let event' = FromEvent { event="click", message=toJSON Up, location=Nothing }

      appliedHandler <- apply chan event' handler

      stateEvent <- atomically $ readTChan chan

      afterState <- apply chan stateEvent appliedHandler

      render afterState
        `shouldBe`
        "<div handler=\"null\">1</div>"

    it "works after sending an event that did not match anything" $ do
      let
        actionHandler :: TestAction -> Int -> (Int, [DirectedEvent String TestAction])
        actionHandler Up   _ = (1, [])
        actionHandler Down _ = (0, [])

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      chan <- newTChanIO

      let event0 = FromEvent { event="init", message="init", location=Nothing }

      appliedHandler0 <- apply chan event0 handler
      render appliedHandler0
        `shouldBe`
        "<div handler=\"null\">0</div>"

      let event1 = FromEvent { event="init", message=toJSON Up, location=Nothing }

      appliedHandler1 <- apply chan event1 appliedHandler0

      stateEvent <- atomically $ readTChan chan
      appliedHandler2 <- apply chan stateEvent appliedHandler1

      render appliedHandler2
        `shouldBe`
        "<div handler=\"null\">1</div>"

    it "works with a nested attribute" $ do
      let
        childHandler :: TestAction -> Int -> (Int, [DirectedEvent String TestAction])
        childHandler Up   _ = (1, [Parent "hello"])
        childHandler Down _ = (0, [])

        parentHandler :: String -> String -> (String, [DirectedEvent String String])
        parentHandler "hello" _ = ("bye", [])
        parentHandler "bye" _ = ("hello", [])
        parentHandler str _ = (str, [])

        styledContainer = style "font-size: 10px;" . div

        handler =
          messageHandler ("" :: String) parentHandler
            $ \message ->
                styledContainer
                [ text message
                , messageHandler (0 :: Int)
                    childHandler
                    (text . show)
                ]

        component = handler

      chan <- newTChanIO

      let
        locatedGraph = fst $ prepareTree component
        event1 = FromEvent { event="click", message=toJSON Up, location=Just [1, 0] }

      afterEvent1 <- apply chan event1 locatedGraph

      receivedEvent1 <- atomically $ readTChan chan
      receivedEvent1 `shouldBe` FromEvent {event = "newState", message = Number 1.0, location = Just [1,0]}

      receivedEvent2 <- atomically $ readTChan chan
      receivedEvent2 `shouldBe` FromEvent {event = "internal", message = String "hello", location = Just []}

    describe "sending events" $ do

      it "can send an event to a parent" $ do
        let
          childHandler :: TestAction -> Int -> (Int, [DirectedEvent String TestAction])
          childHandler Up   _ = (1, [Parent "hello"])
          childHandler Down _ = (0, [])

          parentHandler :: String -> String -> (String, [DirectedEvent String String])
          parentHandler "hello" _ = ("bye", [])
          parentHandler "bye" _ = ("hello", [])
          parentHandler str _ = (str, [])

          handler =
            messageHandler ("" :: String) parentHandler
              $ \message ->
                  div
                  [ text message
                  , messageHandler (0 :: Int)
                      childHandler
                      (text . show)
                  ]

        chan <- newTChanIO

        let locatedGraph = fst $ prepareTree handler

        render locatedGraph `shouldBe` "<div handler=\"[]\"><div><div handler=\"[1,0]\">0</div></div></div>"

        let event1 = FromEvent { event="click", message=toJSON Up, location=Just [1, 0] }

        afterEvent1 <- apply chan event1 locatedGraph

        receivedEvent1 <- atomically $ readTChan chan
        receivedEvent1 `shouldBe` FromEvent {event = "newState", message = Number 1.0, location = Just [1,0]}

        receivedEvent2 <- atomically $ readTChan chan
        -- correctly targeted to the parent
        receivedEvent2 `shouldBe` FromEvent {event = "internal", message = String "hello", location = Just []}


      it "can send an event to self" $ do
        let
          childHandler :: TestAction -> Int -> (Int, [DirectedEvent String TestAction])
          childHandler Up   _ = (1, [Self Down])
          childHandler Down _ = (0, [])

          parentHandler :: String -> String -> (String, [DirectedEvent String String])
          parentHandler "hello" _ = ("bye", [])
          parentHandler "bye" _ = ("hello", [])
          parentHandler str _ = (str, [])

          handler =
            messageHandler ("" :: String) parentHandler
              $ \message ->
                  div
                  [ text message
                  , messageHandler (0 :: Int)
                      childHandler
                      (text . show)
                  ]

        chan <- newTChanIO

        let locatedGraph = fst $ prepareTree handler

        render locatedGraph `shouldBe` "<div handler=\"[]\"><div><div handler=\"[1,0]\">0</div></div></div>"

        let event1 = FromEvent { event="click", message=toJSON Up, location=Just [1, 0] }

        afterEvent1 <- apply chan event1 locatedGraph

        receivedEvent1 <- atomically $ readTChan chan
        receivedEvent1 `shouldBe` FromEvent {event = "newState", message = Number 1.0, location = Just [1,0]}

        receivedEvent2 <- atomically $ readTChan chan
        -- correctly targeted to self
        receivedEvent2 `shouldBe` FromEvent {event = "internal", message = String "Down", location = Just [1,0]}


main :: IO ()
main = hspec spec
