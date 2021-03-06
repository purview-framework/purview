{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|

Purview aims to be pretty straightforward to work with.  As an example,
here's a counter that we'll then go through.

> module Main where
>
> import Purview
>
> incrementButton = onClick "increment" $ button [ text "+" ]
> decrementButton = onClick "decrement" $ button [ text "-" ]
>
> view count = div
>   [ p [ text ("count: " <> show count) ]
>   , incrementButton
>   , decrementButton
>   ]
>
> handler :: (Integer -> Purview String any IO) -> Purview () any IO
> handler = simpleHandler (0 :: Integer) reducer
>
> reducer action state = case action of
>   "increment" -> state + 1
>   "decrement" -> state - 1
>
> top = handler view
>
> main = run defaultConfiguration { component=top, devMode=True }

First we define two buttons, each which have action producers ('onClick').

When rendered, this tells Purview that when either is clicked it'd like to receive
a message ('increment' or 'decrement').

Then we define a handler, which takes an initial state ("0"), and a reducer.

The reducer defines how we're supposed to handle the events received, and it passes
down the new state to components.

Then we put it together ("handler view"), and run it.

Note the "devMode=True": this tells Purview to send the whole
tree over again when the websocket reconnects.  This is really handy
if you're re-running the server in ghci, although I really recommend
using ghcid so you can do:

> ghcid --command 'stack ghci yourProject/Main.hs' --test :main

Which will automatically restart the server on code changes.  It's fast!

For more in depth reading check out the [readme](https://github.com/purview-framework/purview/blob/main/README.md) and
the [examples](https://github.com/purview-framework/purview/tree/main/examples) folder.

-}

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

import           Control.Monad (when)
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent

import           Component
import           EventLoop
import           Events
import           PrepareTree
import           Rendering
import           Wrapper
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)

type Log m = String -> m ()

data Configuration parentAction action m = Configuration
  { component         :: Purview parentAction action m
  -- ^ The top level component to put on the page.
  , interpreter       :: m [Event] -> IO [Event]
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

This starts up the Scotty server.  As a tiny example, to display some text saying "hello":

> import Purview
>
> view = p [ text "hello" ]
>
> main = run defaultConfiguration { component=view }

-}
run :: Monad m => Configuration () any m -> IO ()
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

    Sc.get "/"
      $ Sc.html
      $ LazyText.fromStrict
      $ wrapHtml htmlHead htmlEventHandlers
      $ Data.Text.pack
      $ render . fst
      $ prepareTree routes

webSocketMessageHandler :: TChan Event -> WebSocket.Connection -> IO ()
webSocketMessageHandler eventBus websocketConnection = do
  message' <- WebSocket.receiveData websocketConnection

  case decode message' of
    Just fromEvent -> atomically $ writeTChan eventBus fromEvent
    Nothing -> pure ()

  webSocketMessageHandler eventBus websocketConnection

webSocketHandler
  :: Monad m
  => Bool
  -> (m [Event] -> IO [Event])
  -> Log IO
  -> Purview parentAction action m
  -> WebSocket.ServerApp
webSocketHandler devMode runner log component pending = do
  when devMode $ putStrLn "ws connected"
  conn <- WebSocket.acceptRequest pending

  eventBus <- newTChanIO

  atomically $ writeTChan eventBus $ Event { event = "init", message = "init", location = Nothing }

  WebSocket.withPingThread conn 30 (pure ()) $ do
    _ <- forkIO $ webSocketMessageHandler eventBus conn
    eventLoop devMode runner log eventBus conn component
