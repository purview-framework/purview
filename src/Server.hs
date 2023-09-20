{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import           Network.HTTP.Types ( status200 )
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Builder.Internal
import qualified Data.Text as Text
import           Data.Typeable
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent
import           Data.Aeson
import           Blaze.ByteString.Builder.Char.Utf8

import           Component
import           EventLoop
import           Events
import           PrepareTree
import           Rendering
import           Wrapper
import           CollectInitials
import           CleanTree
import           Configuration

{-|

This starts up the Warp server.  As a tiny example, to display some text saying "hello world":

> import Purview
>
> view _ = p [ text "hello world" ]
>
> main = serve defaultConfiguration view

-}
serve :: Monad m => Configuration m -> (String -> Purview () m) -> IO ()
serve config@Configuration{ port, logger } component =
  let
    settings = Warp.setPort port Warp.defaultSettings
  in do
    logger $ "Starting on port " <> show port

    Warp.runSettings settings
      $ WaiWebSocket.websocketsOr
          WebSocket.defaultConnectionOptions
          (webSocketHandler config component)
          (httpHandler config component)

webSocketHandler
  :: Monad m
  => Configuration m
  -> (String -> Purview () m)
  -> WebSocket.PendingConnection -> IO ()
webSocketHandler config component pendingConnection = do
  let
    path = ByteString.unpack
      $ WebSocket.requestPath (WebSocket.pendingRequest pendingConnection)
    render = component path

  connection <- WebSocket.acceptRequest pendingConnection
  startWebSocketLoop config { devMode=True } render connection

httpHandler :: Configuration m -> (String -> Purview () m) -> Wai.Application
httpHandler config component request respond =
  let
    path = Text.unpack . Text.concat $ Wai.pathInfo request
    render = component $ "/" <> path
  in
    respond
      $ Wai.responseBuilder
          status200
          [("Content-Type", "text/html")]
          (renderFullPage config render)

renderFullPage :: Typeable action => Configuration m -> Purview action m -> Builder
renderFullPage Configuration { htmlHead, eventsToListenTo, eventProducers, eventListeners } component =
  let
    locatedComponent = prepareTree component
    (initialEvents, css) = collectInitials locatedComponent
    rendered = render (cleanTree css locatedComponent)
    wrap = wrapHtml css htmlHead eventsToListenTo eventProducers eventListeners
  in
    fromString $ wrap rendered

startWebSocketLoop
  :: (Monad m, Typeable action)
  => Configuration m
  -> Purview action m
  -> WebSocket.Connection
  -> IO ()
startWebSocketLoop Configuration { devMode, interpreter, logger } component connection = do
  eventBus <- newTChanIO

  atomically
    $ writeTChan eventBus
    $ FromFrontendEvent { kind = "init", childLocation = Nothing, location = Nothing, value = Nothing }

  WebSocket.withPingThread connection 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus connection
    eventLoop devMode interpreter logger eventBus connection component

webSocketMessageHandler :: TChan Event -> WebSocket.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message' <- WebSocket.receiveData websocketConnection

  case decode message' of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> do
      print $ "error: failed to decode event: " <> message'
      print "this may be an error in Purview so feel free to open an issue"
      pure ()

  webSocketMessageHandler eventBus websocketConnection
