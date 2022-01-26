{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Purview
  ( div
  , text
  , onClick
  , Purview (..)
  , run
  -- for testing
  , render
  -- for experiment
  , FromEvent (..)
  )
where

import Prelude hiding (div, log)
import qualified Web.Scotty as Sc
import           Data.Text (Text, pack)
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
      $ render [] routes


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
  log $ "\x1b[34;1mreceived>\x1b[0m " <> show message

  let
    (FromEvent eventKind eventMessage) = message
    (newTree, actions) = runOnces component

  newTree' <- apply eventBus message newTree

  mapM_ (atomically . writeTChan eventBus) actions

  let
    newHtml = render [] newTree'

  log $ "\x1b[32;1msending>\x1b[0m " <> show newHtml

  WS.sendTextData
    connection
    (encode $ Event { event = "setHtml", message = Data.Text.pack newHtml })

  looper log eventBus connection newTree'

webSocketMessageHandler :: TChan FromEvent -> WS.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message <- WS.receiveData websocketConnection

  case decode message of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> pure ()

  webSocketMessageHandler eventBus websocketConnection

webSocketHandler :: Log IO -> Purview a -> WS.ServerApp
webSocketHandler log component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  eventBus <- newTChanIO
  atomically $ writeTChan eventBus $ FromEvent { event = "init", message = "init" }

  WS.withPingThread conn 30 (pure ()) $ do
    forkIO $ webSocketMessageHandler eventBus conn
    looper log eventBus conn component