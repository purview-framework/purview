{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson
import           Data.String (fromString)
import           Data.Typeable

data Attributes = OnClick | Style

data Purview a where
  Attribute :: Attributes -> b -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: a -> Purview a

  State :: state -> ((state, state -> m ()) -> Purview a) -> Purview a
  MessageHandler
    :: (Typeable action)
    => (action -> m ())
    -> ((action -> b) -> Purview a)
    -> Purview a
  Once :: (action -> ()) -> Purview a -> Purview a

-- a little bit to clean up defining these
div = Html "div"
text = Text
useState = State
onClick = Attribute OnClick

renderAttributes :: [Attributes] -> String
renderAttributes = concatMap renderAttribute
  where
    renderAttribute OnClick = " bridge-click=\"clicked\""

{-

Html Tag Children

-}

render :: [Attributes] -> Purview a -> String
render attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concatMap (render attrs) rest <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr x rest ->
    render (attr:attrs) rest

apply :: Typeable b => b -> Purview a -> Purview a
apply action component = undefined
