{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson
import           Data.String (fromString)
import           Data.Typeable
import           GHC.Generics
import           Control.Concurrent
import           Control.Monad
import Debug.Trace

data Attributes where
  -- OnClick :: Typeable a => (a -> IO ()) -> Attributes
  OnClick :: a -> Attributes

data Purview a where
  Attribute :: Attributes -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: a -> Purview a

  State
    :: state
    -> ((state, state -> m ()) -> Purview a)
    -> Purview a

  MessageHandler
    :: (FromJSON action)
    => state
    -> (action -> state)
    -> (state -> Purview a)
    -> Purview a

--  MessageHandler
--    :: (Typeable action)
--    => (action -> IO ())
--    -> ((action -> b) -> Purview a)
--    -> Purview a
  -- Once :: (action -> ()) -> Purview a -> Purview a

-- a little bit to clean up defining these
div = Html "div"
text = Text
useState = State
onClick = Attribute . OnClick

temp state setState = OnClick $ do
  setState 1

renderAttributes :: [Attributes] -> String
renderAttributes = concatMap renderAttribute
  where
    renderAttribute (OnClick _) = " bridge-click=\"click\""

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

  Attribute attr rest ->
    render (attr:attrs) rest

  MessageHandler state _ cont ->
    render attrs (cont state)

-- rewrite :: Purview a -> Purview a
-- rewrite component = case component of
--   Attribute (OnClick fn) cont ->
--     MessageHandler handler (const $ rewrite cont)
--     where
--       handler "RUN" = fn
--   e -> rewrite e

apply :: Value -> Purview a -> Purview a
apply action component = case component of
  MessageHandler state handler cont -> case fromJSON action of
    Success action' ->
      MessageHandler (handler action') handler cont
    Error _ -> cont state
  _ -> error "sup"
