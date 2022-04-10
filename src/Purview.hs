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
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
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

run :: Monad m => (m [FromEvent] -> IO [FromEvent]) -> Log IO -> Purview a m -> IO ()
run runner log routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler' <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr
        WS.defaultConnectionOptions
        (webSocketHandler runner log routes)
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

webSocketMessageHandler :: TChan FromEvent -> WS.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message' <- WS.receiveData websocketConnection

  case decode message' of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> pure ()

  webSocketMessageHandler eventBus websocketConnection

webSocketHandler :: Monad m => (m [FromEvent] -> IO [FromEvent]) -> Log IO -> Purview a m -> WS.ServerApp
webSocketHandler runner log component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  eventBus <- newTChanIO

  atomically $ writeTChan eventBus $ FromEvent { event = "init", message = "init", location = Nothing }

  WS.withPingThread conn 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus conn
    eventLoop runner log eventBus conn component
