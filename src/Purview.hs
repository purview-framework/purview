{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Purview
  ( div
  , form
  , text
  , onClick
  , onSubmit
  , Purview (..)
  , Attributes (..)
  , messageHandler
  , effectHandler
  , run
  -- for testing
  , render
  -- for experiment
  , FromEvent (..)
  )
where

import Prelude hiding (div, log)
import qualified Web.Scotty as Sc
import           Data.Text (pack)
import qualified Data.Text.Lazy as LazyText
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Data.Aeson

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent

import           Component
import           Wrapper
import           Events
import           Diffing


type Log m = String -> m ()

run :: Log IO -> Purview a -> IO ()
run log routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler' <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr
        WS.defaultConnectionOptions
        (webSocketHandler log routes)
        requestHandler'

requestHandler :: Purview a -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    -- Sc.middleware S.logStdoutDev

    Sc.get "/"
      $ Sc.html
      $ LazyText.fromStrict
      $ wrapHtml
      $ Data.Text.pack
      $ render . fst
      $ prepareGraph routes


--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
looper :: Log IO -> TChan FromEvent -> WS.Connection -> Purview a -> IO ()
looper log eventBus connection component = do
  message <- atomically $ readTChan eventBus
  log $ "received> " <> show message

  let
    (newTree, actions) = prepareGraph component

  -- apply can replace state
  newTree' <- apply eventBus message newTree

  mapM_ (atomically . writeTChan eventBus) actions

  let
    -- so I think here, we diff then render the diffs
    diffs = diff [0] component newTree'
    renderedDiffs = fmap (\(Update location graph) -> Update location (render graph)) diffs

  -- log $ "new html> " <> show newTree'
  log $ "sending> " <> show renderedDiffs

  WS.sendTextData
    connection
    (encode $ Event { event = "setHtml", message = renderedDiffs })

  looper log eventBus connection newTree'

webSocketMessageHandler :: TChan FromEvent -> WS.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message' <- WS.receiveData websocketConnection

  case decode message' of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> pure ()

  webSocketMessageHandler eventBus websocketConnection

webSocketHandler :: Log IO -> Purview a -> WS.ServerApp
webSocketHandler log component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  eventBus <- newTChanIO

  atomically $ writeTChan eventBus $ FromEvent { event = "init", message = "init", location = Nothing }

  WS.withPingThread conn 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus conn
    looper log eventBus conn component
