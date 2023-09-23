module Configuration where

import Events

data Configuration m = Configuration
  { interpreter       :: m [Event] -> IO [Event]
  -- ^ How to run your algebraic effects or other.  This will apply to all `effectHandler`s.
  , logger            :: String -> IO ()
  -- ^ Specify what to do with logs
  , eventsToListenTo  :: [String]
  -- ^ For extending the handled events.  By default it covers the usual click, change, focus(in/out)
  , htmlHead          :: String
  -- ^ This is placed directly into the \<head\>, so that you can link to external
  -- CSS etc
  , devMode           :: Bool
  -- ^ When enabled, Purview will send the whole tree on websocket reconnection.
  -- This enables you to use
  -- "ghcid --command 'stack ghci examples/Main.hs' --test :main`"
  -- to restart the server on file change, and get a kind of live reloading
  , javascript        :: String
  , port              :: Int
  -- ^ the port to run on
  , secure            :: Bool
  -- ^ whether the websocket tries to connect using ws:// or wss://
  -- locally you probably want this false, in prod secure of course
  }
