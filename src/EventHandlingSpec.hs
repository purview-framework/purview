{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module EventHandlingSpec where

import Prelude hiding (div)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class
import Test.Hspec
import Data.Aeson
import Data.Aeson.TH
import Data.Time

import Component
import Rendering
import Events
import EventHandling


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
    "newState" -> pure $ applyNewState eventBus fromEvent component
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
        locatedGraph = fst $ prepareGraph component
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

        let locatedGraph = fst $ prepareGraph handler

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

        let locatedGraph = fst $ prepareGraph handler

        render locatedGraph `shouldBe` "<div handler=\"[]\"><div><div handler=\"[1,0]\">0</div></div></div>"

        let event1 = FromEvent { event="click", message=toJSON Up, location=Just [1, 0] }

        afterEvent1 <- apply chan event1 locatedGraph

        receivedEvent1 <- atomically $ readTChan chan
        receivedEvent1 `shouldBe` FromEvent {event = "newState", message = Number 1.0, location = Just [1,0]}

        receivedEvent2 <- atomically $ readTChan chan
        -- correctly targeted to self
        receivedEvent2 `shouldBe` FromEvent {event = "internal", message = String "Down", location = Just [1,0]}

  describe "prepareGraph" $ do

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

      show (fst (prepareGraph component))
        `shouldBe`
        "EffectHandler Just [] Just [] Once True div [  \"Nothing\" Attr div [  \"check time\" ]  ] "

      length (snd (prepareGraph component))
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
        run1 = prepareGraph component
        run2 = prepareGraph (fst run1)
        run3 = prepareGraph (fst run2)

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
        graphWithLocation = fst (prepareGraph component)

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

        graphWithLocation = fst (prepareGraph component)

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


        graphWithLocation = fst (prepareGraph component)

      show graphWithLocation `shouldBe` "Hide EffectHandler Just [] Just [] Hide EffectHandler Just [] Just [0] \"\""


main :: IO ()
main = hspec spec
