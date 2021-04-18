{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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

newtype Attributex
  = OnClick String
  deriving Show

type Tag = String

data Html
  = Html Tag [Attributex] [Html]
  | Text String
  | XComponent (forall a b. Component a b)

renderHtml :: Html -> LazyText.Text
renderHtml (Html tag attrs html) =
  "<" <> fromString tag <> ">"
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

data Route a b  = Route
  { location :: String
  , component :: Component a b
  }

run :: Component a b -> IO ()
run routes = do
  let port = 8001
  let settings = Warp.setPort port Warp.defaultSettings
  requestHandler <- requestHandler routes
  Warp.runSettings settings
    $ WaiWs.websocketsOr WS.defaultConnectionOptions webSocketHandler requestHandler

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

    window.ws = ws;
  }
  connect();
|]

wrapHtml :: LazyText.Text -> LazyText.Text
wrapHtml body =
  "<html><head>"
  <> "<script>" <> websocketScript <> "</script>"
  <> "</head><body>"<> body <> "</body></html>"

-- page = docTypeHtml $ do
--   H.head $ do
--     H.title "Natural numbers"
--     script $ toHtml websocketScript
--   body $ do
--     p "A list of natural numbers:"

runCounter = run counter

renderComponent :: Component a b -> LazyText.Text
renderComponent Component{ render, initialize } =
  renderHtml $ render initialize

requestHandler :: Component a b -> IO Wai.Application
requestHandler routes =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $ Sc.html $ wrapHtml $ renderComponent routes

webSocketHandler :: WS.ServerApp
webSocketHandler pending = do
  putStrLn "ws connected"
  conn <- WS.acceptRequest pending

  WS.withPingThread conn 30 (pure ()) $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ ("initial> " :: Text) <> msg

    forever $ do
      WS.sendTextData conn ("loop data" :: Text)
      threadDelay $ 1 * 1000000
