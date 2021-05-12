{-# LANGUAGE DeriveGeneric #-}
-- | Display a list of links

module Posts where

import Prelude hiding (div)
import Lib

import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import GHC.Generics

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Concurrent

type Error = String

data Post = Post
  { id     :: Int
  , userId :: Int
  , title  :: String
  , body   :: String
  } deriving (Generic, Show)

instance FromJSON Post where

getPosts :: IO (Either Error [Post])
getPosts =
  let
    request = get "https://jsonplaceholder.typicode.com/posts"
  in
    (\result -> eitherDecode $ result ^?! responseBody) <$> request

data State = State
  { loading :: Bool }
  deriving Show

postsView :: TChan State -> State -> (Html State)
postsView up (State loading)= do
  -- state' <- readTChan state
  -- HTTP request

  div [] [ text (show loading) ]


test = do
  state <- atomically newTChan -- (State False)

  forkIO $ do
    threadDelay 5000
    atomically $ writeTChan state $ State True

  forkIO $ do
    threadDelay 15000
    atomically $ writeTChan state $ State False

  forever $ do
    state' <- atomically $ readTChan state
    let render = postsView state state'
    -- html <- atomically $ postsView
    print render
    -- print $ render

  threadDelay 5000000
-- main = run print postsView
