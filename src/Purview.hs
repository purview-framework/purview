{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Purview
  ( div
  , form
  , text
  , style
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
  , DirectedEvent (..)
  )
where

import Prelude hiding (div, log)
import qualified Web.Scotty as Sc
import           Data.Text (pack)
import qualified Data.Text.Lazy as LazyText
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Data.Aeson

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent

import           Component
import           EventLoop
import           Events
import           PrepareTree
import           Rendering
import           Wrapper


type Log m = String -> m ()

run :: Monad m => (m [FromEvent] -> IO [FromEvent]) -> Log IO -> Purview () m -> IO ()
run runner log component = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler' <- requestHandler component
  Warp.runSettings settings
    $ WaiWebSocket.websocketsOr
        WebSocket.defaultConnectionOptions
        (webSocketHandler runner log component)
        requestHandler'

requestHandler :: Purview a m -> IO Wai.Application
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
      $ prepareTree routes

webSocketMessageHandler :: TChan FromEvent -> WebSocket.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message' <- WebSocket.receiveData websocketConnection

  case decode message' of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> pure ()

  webSocketMessageHandler eventBus websocketConnection

webSocketHandler :: Monad m => (m [FromEvent] -> IO [FromEvent]) -> Log IO -> Purview a m -> WebSocket.ServerApp
webSocketHandler runner log component pending = do
  putStrLn "ws connected"
  conn <- WebSocket.acceptRequest pending

  eventBus <- newTChanIO

  atomically $ writeTChan eventBus $ FromEvent { event = "init", message = "init", location = Nothing }

  WebSocket.withPingThread conn 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus conn
    eventLoop runner log eventBus conn component
