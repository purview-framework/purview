{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Display a list of links

module Posts where

import Prelude hiding (div, id)
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

newtype State = State
  { posts :: Maybe [Post] }
  deriving Show

postView :: Post -> Html State
postView Post{id, userId, title, body} =
  div []
  [ text $ show id <> " " <> show userId
  , text title
  , text body
  ]

postsView :: TChan State -> State -> IO (Html State)
postsView up (State posts)= do

  -- HTTP request
  forkIO $ do
    posts <- getPosts
    case posts of
      Right posts -> atomically $ writeTChan up (State (Just posts))
      Left _      -> pure ()

  pure $ case posts of
    Nothing    -> div [] [ text "Loading..." ]
    Just posts -> div [] (fmap postView posts)


test = do
  state <- atomically newTChan -- (State False)

  forever $ do
    state' <- atomically $ readTChan state
    render <- postsView state state'
    -- html <- atomically $ postsView
    print render
    -- print $ render

-- main = run print postsView
