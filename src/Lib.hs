{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String

import           Control.Monad
import qualified Control.Monad.State as MS
import           Control.Concurrent
import           Text.RawString.QQ (r)

-- kind [attr] [more]

run :: IO ()
run = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions webSocketHandler requestHandler

-- | Render some Blaze Html
blaze :: Html -> LazyText.Text
blaze h = LazyText.pack $ renderHtml h

websocketScript :: Text
websocketScript = [r|
  function connect()
  {
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
  }
  connect();
|]

page = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
    script $ toHtml websocketScript
  body $ do
    p "A list of natural numbers:"

requestHandler :: IO Wai.Application
requestHandler =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.html $ blaze page

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
