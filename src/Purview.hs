{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Purview
  (
  -- ** Server
    run
  , Configuration (..)
  , defaultConfiguration

  -- ** Handlers
  -- | These are how you can catch events sent from things like 'onClick' and
  -- change state, or in the case of 'effectHandler', make API requests or call
  -- functions from your project.
  , simpleHandler
  , messageHandler
  , effectHandler

  -- ** HTML helpers
  , div
  , span
  , p
  , h1
  , h2
  , h3
  , h4
  , text
  , button
  , form
  , input
  , style

  -- ** Action producers
  , onClick
  , onSubmit

  -- ** For Testing
  , render

  -- ** AST
  , Attributes (..)
  , DirectedEvent (..)
  , Purview (..)
  )
where

import Prelude hiding (div, log, span)
import qualified Web.Scotty as Sc
import           Data.Text (pack, Text, all)
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
  -- ^ The top level component to put on the page.
  , interpreter       :: m [FromEvent] -> IO [FromEvent]
  -- ^ How to run your algebraic effects or other.  This will apply to all `effectHandler`s.
  , logger            :: String -> IO ()
  -- ^ Specify what to do with logs
  , htmlEventHandlers :: [HtmlEventHandler]
  -- ^ For extending the handled events.  Have a look at 'defaultConfiguration' to see
  -- how to make your own.
  , htmlHead          :: Text
  -- ^ This is placed directly into the \<head\>, so that you can link to external
  -- CSS etc
  , devMode           :: Bool
  -- ^ When enabled, Purview will send the whole tree on websocket reconnection.
  -- This enables you to use
  -- "ghcid --command 'stack ghci examples/Main.hs' --test :main`"
  -- to restart the server on file change, and get a kind of live reloading
  }

defaultConfiguration :: Configuration parentAction action IO
defaultConfiguration = Configuration
  { component         = div []
  , interpreter       = id
  , logger            = print
  , htmlEventHandlers = [clickEventHandler, submitEventHandler]
  , htmlHead          = ""
  , devMode           = False
  }

{-|

This starts up the server.  As a tiny example, to display some text saying "hello":

> import Purview
>
> view = p [ text "hello" ]
>
> main = run defaultConfiguration { component=view }

-}
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
