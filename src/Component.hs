{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import           Data.Aeson
import           Data.String (fromString, IsString)

--
-- How the user can define components
--
data Component state messages = Component
  { state      :: state
  , handlers   :: state -> messages -> state
  , render     :: state -> Html messages
  }

renderAttributes :: Show a => [Attribute a] -> ByteString
renderAttributes = foldr handle ""
  where
    handle (OnClick str) rest = "bridge-click=\""<> fromString (show str) <> "\"" <> rest
    handle (Style str) rest = "style=\""<> str <> "\"" <> rest

renderHtml :: Show a => Html a -> ByteString
renderHtml (Html tag attrs html) =
  "<" <> fromString tag <> " " <> renderAttributes attrs <> ">"
  <> foldr (<>) "" (fmap renderHtml html)
  <> "</" <> fromString tag <> ">"
renderHtml (Text str) = fromString str
renderHtml (SomeComponent comp) = runRender comp


data Attribute a
  = OnClick a
  | Style ByteString
  deriving Show

type Tag = String

class Render m where
  runRender :: m -> ByteString

instance Show m => Render (Component s m) where
  runRender (Component state handler render) =
    renderHtml (render state)

instance Show m => Render (Html m) where
  runRender html = renderHtml html

class Handler m where
  handle :: m -> Value -> m

instance Handler (Html m) where
  handle (Html tag attrs els) message =
    Html tag attrs $ fmap (\sub -> handle sub message) els
  handle (Text str) message = Text str
  handle (SomeComponent a) message = SomeComponent (handle a message)

instance (Show s, FromJSON m) => Handler (Component s m) where
  handle (Component state handler render) message =
    case fromJSON message of
      Success m ->
        let
          newState = handler state m
          newRender = fmap (\sub -> handle sub message) render
        in
          Component newState handler newRender
      Error _ ->
        Component state handler render

instance Show s => Show (Component s m) where
  show (Component st _ _) = show st

data Html a
  = Html Tag [Attribute a] [Html a]
  | Text String
  | forall a. (Render a, Handler a, Show a) => SomeComponent a
