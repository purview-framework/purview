{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import qualified Web.Scotty as Sc
import           Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import Network.Wai.Internal ( Response(ResponseBuilder) )
import qualified Network.Wai.Handler.Warp as Warp

-- import           Text.Blaze.Html5 as H
-- import           Text.Blaze.Html5.Attributes as A hiding (id)
-- import           Text.Blaze.Html.Renderer.String

import           Control.Monad
import qualified Control.Monad.State as MS
import           Control.Concurrent
import           Text.RawString.QQ (r)
import           Data.Aeson
import           GHC.Generics
import           Data.String (fromString)

-- kind [attr] [more]

-- goal:
-- get a counter working
--data

newtype Attribute
  = OnClick String
  deriving Show

type Tag = String

data Html
  = Html Tag [Attribute] [Html]
  | Text String
  | XComponent (forall a b. Component a b)

renderAttributes :: [Attribute] -> LazyText.Text
renderAttributes = foldr handle ""
  where
    handle (OnClick str) rest = "bridge-click=\""<> fromString str <> "\"" <> rest

renderHtml :: Html -> LazyText.Text
renderHtml (Html tag attrs html) =
  "<" <> fromString tag <> " " <> renderAttributes attrs <> ">"
  <> foldr (<>) "" (fmap renderHtml html)
  <> "</" <> fromString tag <> ">"
renderHtml (Text str) = fromString str

text = Text

data Component state messages = Component
  { initialize :: state
  , handlers   :: state -> messages -> state
  , render     :: state -> Html
  }

defaultComponent = Component
  { initialize = id
  , handlers   = const
  , render     = \state -> Html "p" [] [text "default"]
  }

--
-- My app
--
newtype Counter = Counter
  { count :: Int } deriving Show

defaultCounterState = Counter { count = 0 }

counter = defaultComponent
  { initialize = defaultCounterState
  , handlers = \state message ->
      case message of
        "increment" -> state { count = count state + 1 }
        "decrement" -> state { count = count state - 1 }
  , render = \state ->
      Html "div" []
      [ Html "div" [OnClick "increment"] [text "increment"]
      , text ("count: " <> show (count state))
      , Html "div" [OnClick "decrement"] [text "decrement"]
      ]
  }
--
-- End
--

-- data Route a b  = Route
--   { location :: String
--   , component :: Component a b
--   }

websocketScript = [r|
  function connect() {
    var ws = new WebSocket("ws://localhost:8001");

    ws.onopen = () => {
      ws.send("initial from js");
    };

    ws.onmessage = evt => {
      var m = evt.data;
      console.log( m );
      console.log(JSON.parse( m ));
      var event = JSON.parse(evt.data);
      if (event.event === "setHtml") {
        document.body.innerHTML = event.message;
      }
    };

    ws.onclose = function() {
      alert("ws closed");
    };

    window.onbeforeunload = evt => {
      ws.close();
    };

    window.ws = ws;
  }
  connect();

  function logEvents(event) {
    var clickValue = event.target.getAttribute("bridge-click");
    if (clickValue) {
      window.ws.send(JSON.stringify({ "event": "click", "message": clickValue }));
    }
  }

  function bindEvents() {
    document.getRootNode().addEventListener("click", logEvents);
  }
  bindEvents();
|]

wrapHtml :: LazyText.Text -> LazyText.Text
wrapHtml body =
  "<html><head>"
  <> "<script>" <> websocketScript <> "</script>"
  <> "</head><body>"<> body <> "</body></html>"

runCounter = run counter

renderComponent :: Component a b -> LazyText.Text
renderComponent Component{ render, initialize } =
  renderHtml $ render initialize

run :: Component a String -> IO ()
run routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions (webSocketHandler routes) requestHandler

requestHandler :: Component a String -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $ Sc.html $ wrapHtml $ renderComponent routes


data Event = Event
  { event :: String
  , message :: String
  } deriving (Generic, Show)

instance FromJSON Event where
instance ToJSON Event where

webSocketHandler :: Component a String -> WS.ServerApp
webSocketHandler component pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg <- WS.receiveData conn

      let event = (decode msg :: Maybe Event)
          newHtml = case event of
            Nothing -> renderComponent component
            Just event ->
              let newState = (handlers component) (initialize component) (message event)
              in renderComponent (component { initialize = newState })

      print $ ("msg> " :: Text) <> fromString (show event)

      WS.sendTextData
        conn
        (encode $ Event { event = "setHtml", message = LazyText.unpack newHtml })

      -- threadDelay $ 1 * 1000000
