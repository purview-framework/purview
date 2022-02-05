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

{-|

Takes the tree and turns it into HTML.  Attributes are passed down to children until
they reach a real HTML tag.

-}

render' :: [Integer] -> [Attributes] -> Purview a -> String
render' location attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concatMap (\(newLocation, comp) -> render' (newLocation:location) attrs comp) (zip [0..] rest) <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr rest ->
    render' location (attr:attrs) rest

  MessageHandler state _ cont ->
    "<div handler=" <> show (encode location) <> ">" <>
      render' (0:location) attrs (cont state) <>
    "</div>"

  EffectHandler state _ cont ->
    "<div handler=" <> show (encode location) <> ">" <>
      render' (0:location) attrs (cont state) <>
    "</div>"

  Once _ hasRun cont ->
    render' location attrs cont

render :: Purview a -> String
render = render' [0] []

{-|

This is a special case event to assign state to message handlers

-}

applyNewState :: TChan FromEvent -> Value -> Purview a -> IO (Purview a)
applyNewState eventBus message component = case component of
  MessageHandler state handler cont -> pure $ case fromJSON message of
    Success newState ->
      MessageHandler newState handler cont
    Error _ ->
      cont state

  EffectHandler state handler cont -> case fromJSON message of
    Success newState -> do
      pure $ EffectHandler newState handler cont
  x -> pure x

applyEvent :: TChan FromEvent -> Value -> Purview a -> IO (Purview a)
applyEvent eventBus message component = case component of
  MessageHandler state handler cont -> pure $ case fromJSON message of
    Success action' ->
      MessageHandler (handler action' state) handler cont
    Error _ ->
      MessageHandler state handler cont

  EffectHandler state handler cont -> case fromJSON message of
    Success parsedAction -> do
      void . forkIO $ do
        newState <- handler parsedAction state
        atomically $ writeTChan eventBus $ FromEvent
          { event = "newState"
          , message = toJSON newState
          }
      pure $ EffectHandler state handler cont
    Error err ->
      pure $ EffectHandler state handler cont

  x -> pure x

{-

For now the only special event kind is "newState" which replaces
the inner state of a Handler (Message or Effect)

-}

apply :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
apply eventBus (FromEvent eventKind message) component =
  case eventKind of
    "newState" -> applyNewState eventBus message component
    _          -> applyEvent eventBus message component

{-

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

-}

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
