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
import Data.Text
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
-- wsapp :: TChan ServerMsg -> WS.ServerApp
-- wsapp fromControl pending = do
--   putStrLn "ws connected"
--   conn <- WS.acceptRequest pending
--   WS.forkPingThread conn 30
--
--   (msg :: Txt.Text) <- WS.receiveData conn
--   WS.sendTextData conn $ ("initial> " :: Txt.Text) <> msg
--
--   forever $ do
--     msg <- atomically $ readTChan fromControl
--     WS.sendTextData conn $ (decodeUtf8 (encode msg))
--     threadDelay $ 1 * 1000000

-- scottyApp :: IO Wai.Application
-- scottyApp =
--   Sc.scottyApp $ do
--     Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
--     --Sc.middleware S.logStdoutDev
--
--     Sc.get "/" $
--       Sc.file "./repl/index.html"

-- runServer :: (TChan ServerMsg) -> IO ()
-- runServer fromControl = do
--   let port = 8000
--   let settings = Warp.setPort port Warp.defaultSettings
--   sapp <- scottyApp
--   Warp.runSettings settings
--     $ WaiWs.websocketsOr
--     WS.defaultConnectionOptions
--     (wsapp fromControl)
--     sapp

-- runServer :: (TChan ServerMsg) -> IO ()

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
    print $ "Sending: " <> message
    hPutStrLn hin message

  -- read output
  forever $ do
    ready <- tryBool isEOFError $ hReady hout
    case ready of
      Right True -> do
        print =<< hGetLine hout
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
  hSetBuffering hout' LineBuffering

  return (hin', hout', err')

--
-- Files
--
handleFileEvent :: EventChannel -> IO ()
handleFileEvent event = do
  evt <- readChan event
  putStrLn . show $ evt
  handleFileEvent event

runFileWatcher :: IO ()
runFileWatcher = do
  fileEventChan <- newChan
  forkIO $ handleFileEvent fileEventChan
  withManager $ \mgr -> do
    watchDirChan
      mgr
      "./"
      (const True)
      fileEventChan
      -- print
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
  -- putStrLn $ "> " <> s

-- handleCode :: String -> ServerMsg
-- handleCode raw =
--   let
--     syntax'        = syntax . (clex 0 0) $ raw
--     compiled'      = compile syntax'
--     (fnName, _, _) = head syntax'
--   in
--     ServerMsg {
--       name=fnName
--       , code=compiled'
--     }

--
-- Connect the dots
--
setupCommand = "import Control.Concurrent"
startCommand = "server <- forkIO $ main"
endCommand   = "killThread server"

main :: IO ()
main = do
  toServer <- atomically (newTChan :: STM (TChan String))

  void . forkIO $ runServer toServer

  fromRepl <- atomically (newTChan :: STM (TChan ReplEvent))
  threadDelay 10000000

  threadDelay 1000000
  print "setup"
  atomically $ writeTChan toServer setupCommand

  threadDelay 1000000
  print "start"
  atomically $ writeTChan toServer startCommand

  threadDelay 1000000
  print "kill"
  atomically $ writeTChan toServer endCommand

--   toRepl   <- atomically (newTChan :: STM (TChan String))
--
--   fromFileWatcher <- atomically (newTChan :: STM (TChan String))
--
--   fromServer <- atomically (newTChan :: STM (TChan String))
--   toServer   <- atomically (newTChan :: STM (TChan ServerMsg))
--
--   -- run the stuff
--   fileWatchPid <- forkIO runFileWatcher
--   -- serverPid    <- forkIO (runServer toServer)

  replPid <- forkIO (repl fromRepl)

  forever $ do
    msg <- atomically $ readTChan fromRepl
    case msg of
      Command ":q" -> do
        -- mapM_ killThread [fileWatchPid, replPid]
        -- mapM_ killThread [fileWatchPid, serverPid, replPid]
        putStrLn "-- shutdown --"
      Code s -> do
        putStrLn $ "code " <> s
        -- atomically $ writeTChan toServer (handleCode s)
        pure ()
