{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Purview
  ( Attributes (..)
  , Purview (..)
  , Configuration (..)
  , defaultConfiguration
  , simpleHandler
  , effectHandler
  , messageHandler
  , div
  , span
  , p
  , h1
  , h2
  , h3
  , h4
  , button
  , form
  , onClick
  , onSubmit
  , style
  , text
  , run
  -- for testing
  , render
  -- for experiment
  , FromEvent (..)
  , DirectedEvent (..)
  )
where

import Prelude hiding (div, log, span)
import qualified Web.Scotty as Sc
import           Data.Text (pack, Text)
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

data Configuration parentAction action m = Configuration
  { component         :: Purview parentAction action m
  , interpreter       :: m [FromEvent] -> IO [FromEvent]
  , logger            :: String -> IO ()
  , htmlEventHandlers :: [HtmlEventHandler]
  , htmlHead          :: Text
  , devMode           :: Bool
  }

defaultConfiguration :: Configuration parentAction action IO
defaultConfiguration = Configuration
  { component         = div []
  , interpreter       = id
  , logger            = print
  , htmlEventHandlers = [clickEventHandler, submitEventHandler]
  , htmlHead          = ""
  -- on websocket reconnect, send the whole page
  , devMode           = False
  }

run :: Monad m => Configuration () () m -> IO ()
run Configuration { devMode, component, logger, interpreter, htmlEventHandlers, htmlHead } = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler' <- requestHandler component htmlHead htmlEventHandlers
  Warp.runSettings settings
    $ WaiWebSocket.websocketsOr
        WebSocket.defaultConnectionOptions
        (webSocketHandler devMode interpreter logger component)
        requestHandler'

requestHandler :: Purview parentAction action m -> Text -> [HtmlEventHandler] -> IO Wai.Application
requestHandler routes htmlHead htmlEventHandlers =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }

    -- Sc.middleware S.logStdoutDev

    Sc.get "/"
      $ Sc.html
      $ LazyText.fromStrict
      $ wrapHtml htmlHead htmlEventHandlers
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

webSocketHandler
  :: Monad m
  => Bool
  -> (m [FromEvent] -> IO [FromEvent])
  -> Log IO
  -> Purview parentAction action m
  -> WebSocket.ServerApp
webSocketHandler devMode runner log component pending = do
  putStrLn "ws connected"
  conn <- WebSocket.acceptRequest pending

  eventBus <- newTChanIO

  atomically $ writeTChan eventBus $ FromEvent { event = "init", message = "init", location = Nothing }

  WebSocket.withPingThread conn 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus conn
    eventLoop devMode runner log eventBus conn component
