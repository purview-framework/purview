{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude hiding (div)
import qualified Web.Scotty as Sc
import           Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
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
import           Data.String (fromString)

newtype Attribute
  = OnClick String
  deriving Show

type Tag = String

data Html
  = Html Tag [Attribute] [Html]
  | Text String
  | XComponent (forall a b. Component a b) -- haven't tested this out yet

renderAttributes :: [Attribute] -> Text
renderAttributes = foldr handle ""
  where
    handle (OnClick str) rest = "bridge-click=\""<> fromString str <> "\"" <> rest

renderHtml :: Html -> Text
renderHtml (Html tag attrs html) =
  "<" <> fromString tag <> " " <> renderAttributes attrs <> ">"
  <> foldr (<>) "" (fmap renderHtml html)
  <> "</" <> fromString tag <> ">"
renderHtml (Text str) = fromString str

text = Text
html = Html
onClick = OnClick
div = Html "div"

--
-- How the user can define components
--
data Component state messages = Component
  { state      :: state
  , handlers   :: state -> messages -> state
  , render     :: state -> Html
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

renderComponent :: Component a b -> Text
renderComponent Component{ render, state } =
  renderHtml $ render state

type Log m = String -> m ()

run :: Log IO -> Component a Text -> IO ()
run log routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions (webSocketHandler log routes) requestHandler

requestHandler :: Component a Text -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $ Sc.html $ LazyText.fromStrict $ wrapHtml $ renderComponent routes


data Event = Event
  { event :: Text
  , message :: Text
  } deriving (Generic, Show)

instance FromJSON Event where
instance ToJSON Event where

--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
looper log conn component = do
  msg <- WS.receiveData conn
  log $ "\x1b[34mreceived>\x1b[0m " <> unpack msg

  let event = (decode msg :: Maybe Event)
      newComponent = case event of
        Nothing -> component
        Just event ->
          let
            newState = handlers component (state component) (message event)
          in
            component { state = newState }

      newHtml = renderComponent newComponent

  log $ "\x1b[32msending>\x1b[0m " <> show newHtml

  WS.sendTextData
    conn
    (encode $ Event { event = "setHtml", message = newHtml })

  looper log conn newComponent


webSocketHandler :: Log IO -> Component a Text -> WS.ServerApp
webSocketHandler log component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  WS.withPingThread conn 30 (pure ()) $ do
    looper log conn component
