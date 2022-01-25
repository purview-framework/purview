{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Aeson
import           Data.String (fromString)
import           Data.Typeable
import           GHC.Generics
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Monad
import Debug.Trace

-- For monad effects
import Control.Concurrent

import Events

data Attributes where
  -- OnClick :: Typeable a => (a -> IO ()) -> Attributes
  OnClick :: ToJSON a => a -> Attributes

data Purview a where
  Attribute :: Attributes -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: Show a => a -> Purview a

  State
    :: state
    -> ((state, state -> m ()) -> Purview a)
    -> Purview a

  MessageHandler
    :: (FromJSON action, FromJSON state)
    => state
    -> (action -> state -> state)
    -> (state -> Purview a)
    -> Purview a

  EffectHandler
    :: (FromJSON action, FromJSON state, ToJSON state)
    => state
    -> (action -> state -> IO state)
    -> (state -> Purview a)
    -> Purview a

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview a
    -> Purview a

instance Show (Purview a) where
  show (EffectHandler state action cont) = "EffectHandler " <> show (cont state)
  show (MessageHandler state action cont) = "MessageHandler " <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value

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

onClick :: ToJSON a => a -> Purview b -> Purview b
onClick = Attribute . OnClick

renderAttributes :: [Attributes] -> String
renderAttributes = concatMap renderAttribute
  where
    renderAttribute (OnClick action) = " bridge-click=" <> unpack (encode action)

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

  EffectHandler state _ cont ->
    render attrs (cont state)

  Once _ hasRun cont ->
    render attrs cont

-- rewrite :: Purview a -> Purview a
-- rewrite component = case component of
--   Attribute (OnClick fn) cont ->
--     MessageHandler handler (const $ rewrite cont)
--     where
--       handler "RUN" = fn
--   e -> rewrite e

apply :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
apply eventBus (FromEvent "newState" message) component = case component of
  MessageHandler state handler cont -> pure $ case fromJSON message of
    Success newState ->
      MessageHandler newState handler cont
    Error _ ->
      cont state

  EffectHandler state handler cont -> case fromJSON message of
    Success newState -> do
      pure $ EffectHandler newState handler cont
  x -> pure x

apply eventBus (FromEvent eventKind message) component = case component of
  MessageHandler state handler cont -> pure $ case fromJSON message of
    Success action' ->
      MessageHandler (handler action' state) handler cont
    Error _ ->
      cont state

  EffectHandler state handler cont -> case fromJSON message of
    Success parsedAction -> do
      void . forkIO $ do
        newState <- handler parsedAction state
        atomically $ writeTChan eventBus $ FromEvent
          { event = "newState"
          , message = toJSON newState
          }

      pure $ EffectHandler state handler cont
    Error _ ->
      pure $ cont state

  -- Once send cont -> undefined

  x -> pure x

runOnces :: Purview a -> (Purview a, [FromEvent])
runOnces component = case component of
  Attribute attrs cont ->
    let result = runOnces cont
    in (Attribute attrs (fst result), snd result)

  Html kind children ->
    let result = fmap runOnces children
    in (Html kind (fmap fst result), concatMap snd result)

  MessageHandler state handler cont ->
    let
      rest = fmap runOnces cont
    in
      (MessageHandler state handler (\state -> fst (rest state)), snd (rest state))

  EffectHandler state handler cont ->
    let
      rest = fmap runOnces cont
    in
      (EffectHandler state handler (\state -> fst (rest state)), snd (rest state))

  Once effect hasRun cont ->
    let send message =
          FromEvent
            { event = "once"
            , message = toJSON message
            }
    in if not hasRun then
        let
          rest = runOnces cont
        in
          (Once effect True (fst rest), [effect send] <> (snd rest))
       else
        let
          rest = runOnces cont
        in
          (Once effect True (fst rest), snd rest)

  component -> (component, [])
