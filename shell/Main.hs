{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import System.IO
import System.IO.Error

import GHC.Generics

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Data.Text hiding (isInfixOf)
import Data.List (isInfixOf)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- file watching
import System.FSNotify

import Data.Aeson
import Data.Maybe (fromJust)

import Lib

data ServerMsg = ServerMsg
  { name :: String
  , code :: String
  } deriving Generic

instance ToJSON ServerMsg where

--
-- Server
--
bool :: (e -> Bool) -> (e -> Maybe e)
bool f x = if f x then Just x else Nothing

tryBool :: Exception e => (e -> Bool) -> IO a -> IO (Either e a)
tryBool f = tryJust (bool f)

runServer :: TChan String -> IO ()
runServer messages = do
  (hin, hout, err) <- mkProcess ""

  -- send things to stdin
  forkIO $ forever $ do
    message <- atomically $ readTChan messages
    putStrLn $ "ghci> " <> message
    hPutStrLn hin message

  -- read output
  forever $ do
    ready <- tryBool isEOFError $ hReady hout
    case ready of
      Right True ->
        hGetLine hout >>= putStrLn
      _ -> pure ()

    threadDelay 50000

mkProcess :: FilePath -> IO (Handle, Handle, Handle)
mkProcess tsserverLocation = do
  (hin, hout, err, pid) <- createProcess_ "ghci"
    (proc "stack"
          [ "ghci"
          , "--main-is"
          , "bridge:exe:bridge-exe"
          ])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , create_group = True
    }

  let (hin', hout', err') = (fromJust hin, fromJust hout, fromJust err)

  hSetBuffering hin' LineBuffering
  hSetBuffering err' LineBuffering
  -- hSetBuffering hout' LineBuffering

  return (hin', hout', err')

--
-- Files
--
isEventRelevant :: System.FSNotify.Event -> Bool
isEventRelevant (Modified file _ _) = True
isEventRelevant _                   = False

handleFileEvent :: TChan String -> EventChannel -> IO ()
handleFileEvent up eventChannel = do
  evt <- readChan eventChannel

  if isEventRelevant evt then
    atomically $ writeTChan up "reload"
  else
    pure ()

  handleFileEvent up eventChannel

runFileWatcher :: TChan String -> IO ()
runFileWatcher up = do
  fileEventChan <- newChan
  forkIO $ handleFileEvent up fileEventChan

  withManager $ \mgr -> do
    watchTreeChan
      mgr
      "./"
      (const True)
      fileEventChan

    forever $ threadDelay 1000000

--
-- Repl
--
data ReplEvent =
  Command String
  | Code String

repl :: TChan ReplEvent -> IO ()
repl up = forever $ do
  s <- getLine
  case s of
    ":q" -> atomically $ writeTChan up (Command ":q") -- write msg kill
    _    -> atomically $ writeTChan up (Code s)

--
-- Connect the dots
--
setupCommand  = "import Control.Concurrent"
startCommand  = "server <- forkIO $ main' putStrLn"  -- TODO: main' bleh
endCommand    = "killThread server"
reloadCommand = ":r"

main :: IO ()
main = do
  toServer <- atomically (newTChan :: STM (TChan String))

  void . forkIO $ runServer toServer

  fromRepl <- atomically (newTChan :: STM (TChan ReplEvent))
  threadDelay 5000000

  threadDelay 1000000
  putStrLn "--- Setup Complete ---"
  atomically $ writeTChan toServer setupCommand

  threadDelay 1000000
  putStrLn "--- Server Coming Online ---"
  atomically $ writeTChan toServer startCommand
--
--  threadDelay 1000000
--  print "kill"
--  atomically $ writeTChan toServer endCommand

--   toRepl   <- atomically (newTChan :: STM (TChan String))
--
  fromFileWatcher <- atomically (newTChan :: STM (TChan String))
--
--   fromServer <- atomically (newTChan :: STM (TChan String))
--   toServer   <- atomically (newTChan :: STM (TChan ServerMsg))
--
--   -- run the stuff
  fileWatchPid <- forkIO $ runFileWatcher fromFileWatcher
--   -- serverPid    <- forkIO (runServer toServer)

  replPid <- forkIO (repl fromRepl)

  fileWatchHandlerPid <- forkIO $ forever $ do
    event <- atomically $ readTChan fromFileWatcher
    case event of
      "reload" -> do
        putStrLn "\x1b[31mServer Reloading\x1b[0m"
        mapM_ (atomically . writeTChan toServer)
          [ endCommand
          , reloadCommand
          , startCommand
          ]
      _ -> pure ()

  forever $ do
    msg <- atomically $ readTChan fromRepl
    case msg of
      Command ":q" -> do
        mapM_ killThread [fileWatchPid, fileWatchHandlerPid, replPid]
        -- mapM_ killThread [fileWatchPid, serverPid, replPid]
        putStrLn "-- shutdown --"
      Code s -> do
        putStrLn $ "code " <> s
        -- atomically $ writeTChan toServer (handleCode s)
        pure ()
