{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String

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

-- Generic stuff
newtype Message b = Message b
  -- Hello

data Component state b = Component
  { initialize :: state
  , handlers   :: state -> Message b -> state
  , render     :: state -> Html
  }

data Route state b = Route
  { location :: String
  , component :: Component state b
  }

type Routes a b = [Route a b]

-- My app
data Todo = Todo
  { name :: String
  , done :: Bool
  } deriving (Generic, Show)

newtype TodoState = TodoState [Todo]

defaultTodoState = TodoState []

data MyMessages = Hello

todoComponent = Component
  { initialize = defaultTodoState
  , handlers = \state (Message message) ->
      case message of
        Hello -> state
  , render = \state -> p "hello"
  }

todo = [ Route "/" todoComponent ]

runTodo = run todo

run :: Routes a b -> IO ()
run routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions webSocketHandler requestHandler

-- | Render some Blaze Html
blaze :: Html -> LazyText.Text
blaze h = LazyText.pack $ renderHtml h

websocketScript :: Text
websocketScript = [r|
  function connect() {
    var ws = new WebSocket("ws://localhost:8001");

    ws.onopen = () => {
      ws.send("initial from js");
    };

    ws.onmessage = evt => {
      var m = evt.data;
      console.log( m );
    };

    ws.onclose = function() {
      alert("ws closed");
    };

    window.onbeforeunload = evt => {
      socket.close();
    };
    global.ws = ws;
  }
  connect();
|]

page = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
    script $ toHtml websocketScript
  body $ do
    p "A list of natural numbers:"

renderComponent :: Component a b -> LazyText.Text
renderComponent Component{ render, initialize } = blaze $ render initialize

requestHandler :: Routes a b -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    mapM_
      (\Route{ location, component } ->
          Sc.get (fromString location) $ Sc.html $ renderComponent component) routes

webSocketHandler :: WS.ServerApp
webSocketHandler pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  WS.withPingThread conn 30 (pure ()) $ do
    (msg :: Text) <- WS.receiveData conn
    WS.sendTextData conn $ ("initial> " :: Text) <> msg

    forever $ do
      WS.sendTextData conn ("loop data" :: Text)
      threadDelay $ 1 * 1000000
