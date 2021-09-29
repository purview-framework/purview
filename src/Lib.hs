{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude hiding (div)
import qualified Web.Scotty as Sc
import           Data.Text (Text, pack)
import qualified Data.Text.Lazy as LazyText
import           Data.Text.Encoding
import qualified Data.ByteString as DB
import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import Network.Wai.Internal ( Response(ResponseBuilder) )
import qualified Network.Wai.Handler.Warp as Warp

import           Control.Monad
import           Control.Concurrent
import           Text.RawString.QQ (r)
import           Data.Aeson
import           GHC.Generics
import           Data.String (fromString, IsString)
import           Debug.Trace

data Attribute a
  = OnClick a
  | Style ByteString
  deriving Show

type Tag = String

class Render m where
  runRender :: m -> ByteString

instance Show m => Render (Component s m) where
  runRender (Component state handler render) =
    renderHtml (render state)

instance Show m => Render (Html m) where
  runRender html = renderHtml html

class Handler m where
  handle :: m -> Value -> m

instance Handler (Html m) where
  handle (Html tag attrs els) message =
    Html tag attrs $ fmap (\sub -> handle sub message) els
  handle (Text str) message = Text str
  handle (SomeComponent a) message = SomeComponent (handle a message)

instance (Show s, FromJSON m) => Handler (Component s m) where
  handle (Component state _ _) _ | trace ("render " <> show state) False = undefined
  handle (Component state handler render) message =
    case fromJSON message of
      Success m ->
        let
          newState = handler state m
          newRender = fmap (\sub -> handle sub message) render
        in
          Component newState handler newRender
      Error _ ->
        Component state handler render

instance Show s => Show (Component s m) where
  show (Component st _ _) = show st

data Html a
  = Html Tag [Attribute a] [Html a]
  | Text String
  | forall a. (Render a, Handler a, Show a) => SomeComponent a

renderAttributes :: Show a => [Attribute a] -> ByteString
renderAttributes = foldr handle ""
  where
    handle (OnClick str) rest = "bridge-click=\""<> fromString (show str) <> "\"" <> rest
    handle (Style str) rest = "style=\""<> str <> "\"" <> rest

renderHtml :: Show a => Html a -> ByteString
renderHtml (Html tag attrs html) =
  "<" <> fromString tag <> " " <> renderAttributes attrs <> ">"
  <> foldr (<>) "" (fmap renderHtml html)
  <> "</" <> fromString tag <> ">"
renderHtml (Text str) = fromString str
renderHtml (SomeComponent comp) = runRender comp

text = Text
html = Html
onClick = OnClick
style = Style
div = Html "div"

--
-- How the user can define components
--
data Component state messages = Component
  { state      :: state
  , handlers   :: state -> messages -> state
  , render     :: state -> Html messages
  }

defaultComponent = Component
  { state    = id
  , handlers = const
  , render   = \state -> Html "p" [] [text "default"]
  }

--
-- Handling for connecting, sending events, and replacing html
--
-- For now it's nothing fancy with a single event binding on the
-- top level <html> element, which I copied from phoenix live view.
--
-- Definitely easier than dealing with binding/unbinding when updating
-- the html.
--
websocketScript = [r|
  var timeoutTime = -50;
  function connect() {
    timeoutTime += 50;
    var ws = new WebSocket("ws://localhost:8001");

    ws.onopen = () => {
      ws.send("initial from js");
      timeoutTime = 0;
    };

    ws.onmessage = evt => {
      var m = evt.data;
      console.log( m );
      console.log(JSON.parse( m ));
      var event = JSON.parse(evt.data);
      if (event.event === "setHtml") {
        // cool enough for now
        document.body.innerHTML = event.message;
      }
    };

    ws.onclose = function() {
      setTimeout(function() {
        console.debug("Attempting to reconnect");
        connect();
      }, timeoutTime);
    };

    window.onbeforeunload = evt => {
      ws.close();
    };

    window.ws = ws;
  }
  connect();

  function handleEvents(event) {
    var clickValue = event.target.getAttribute("bridge-click");
    if (clickValue) {
      window.ws.send(JSON.stringify({ "event": "click", "message": clickValue }));
    }
  }

  function bindEvents() {
    document.getRootNode().addEventListener("click", handleEvents);
  }
  bindEvents();
|]

wrapHtml :: Text -> Text
wrapHtml body =
  "<html>"
  <> "<head>"
  <> "<script>" <> websocketScript <> "</script>"
  <> "</head>"
  <> "<body>"<> body <> "</body>"
  <> "</html>"

renderComponent :: Show a => Html a -> ByteString
renderComponent = runRender

type Log m = String -> m ()

run :: Show a => Log IO -> Html a -> IO ()
run log routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions (webSocketHandler log routes) requestHandler

requestHandler :: Show a => Html a -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $ Sc.html $ LazyText.fromStrict $ wrapHtml $ (decodeUtf8 . toStrict $ renderComponent routes)

data Event = Event
  { event :: Text
  , message :: Text
  } deriving (Generic, Show)

data FromEvent = FromEvent
  { event :: Text
  , message :: Value
  }

instance FromJSON FromEvent where
  parseJSON (Object o) =
      FromEvent <$> o .: "event" <*> (o .: "message")

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
looper :: Show a => Log IO -> WS.Connection -> Html a -> IO ()
looper log conn component = do
  msg <- WS.receiveData conn
  log $ "\x1b[34;1mreceived>\x1b[0m " <> unpack msg

  let
    decoded = decode msg :: Maybe FromEvent
    newTree = case decoded of
      Just (FromEvent _ message) -> handle component message
      Nothing -> component

    newHtml = renderComponent newTree

  log $ "\x1b[32;1msending>\x1b[0m " <> show newHtml

  WS.sendTextData
    conn
    (encode $ Event { event = "setHtml", message = decodeUtf8 . toStrict $ newHtml })

  looper log conn newTree


webSocketHandler :: Show a => Log IO -> Html a -> WS.ServerApp
webSocketHandler log component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  WS.withPingThread conn 30 (pure ()) $ do
    looper log conn component
