{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
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
import           Data.Maybe (fromJust)
import           GHC.Generics
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Monad
import Debug.Trace

-- For monad effects
import Control.Concurrent

import Events

data Attributes action where
  OnClick :: ToJSON action => action -> Attributes action

type Identifier = Maybe [Int]

data Purview a where
  Attribute :: Attributes a -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: Show a => a -> Purview a

  State
    :: state
    -> ((state, state -> m ()) -> Purview a)
    -> Purview a

  MessageHandler
    :: (FromJSON action, FromJSON state)
    => Identifier
    -> state
    -> (action -> state -> state)
    -> (state -> Purview a)
    -> Purview a

  EffectHandler
    :: (FromJSON action, FromJSON state, ToJSON state)
    => Identifier
    -> state
    -> (action -> state -> IO state)
    -> (state -> Purview action)
    -> Purview action

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview a
    -> Purview a

  Hide :: Purview b -> Purview a

instance Show (Purview a) where
  show (EffectHandler location state action cont) = "EffectHandler " <> show location <> " " <> show (cont state)
  show (MessageHandler location state action cont) = "MessageHandler " <> show location <> " " <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value
  show (Hide a) = show a

instance Eq (Purview a) where
  a == b = show a == show b

-- Various helpers
div = Html "div"
text = Text
useState = State

messageHandler state handler = Hide . EffectHandler Nothing state (\action state -> pure $ handler action state)
effectHandler state handler = Hide . EffectHandler Nothing state handler

onClick :: ToJSON b => b -> Purview b -> Purview b
onClick = Attribute . OnClick

renderAttributes :: [Attributes a] -> String
renderAttributes = concatMap renderAttribute
  where
    renderAttribute (OnClick action) = " action=" <> unpack (encode action)

{-|

Takes the tree and turns it into HTML.  Attributes are passed down to children until
they reach a real HTML tag.

-}

render :: Purview a -> String
render = render' []

render' :: [Attributes a] -> Purview a -> String
render' attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concatMap (render' attrs) rest <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr rest ->
    render' (attr:attrs) rest

  MessageHandler location state _ cont ->
    "<div handler=" <> (show . encode) location <> ">" <>
      render' attrs (cont state) <>
    "</div>"

  EffectHandler location state _ cont ->
    "<div handler=" <> (show . encode) location <> ">" <>
      render' attrs (cont state) <>
    "</div>"

  Once _ hasRun cont ->
    render' attrs cont

{-|

This is a special case event to assign state to message handlers

-}

applyNewState :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
applyNewState eventBus FromEvent { message } component = case component of
  MessageHandler loc state handler cont -> pure $ case fromJSON message of
    Success newState ->
      MessageHandler loc newState handler cont
    Error _ ->
      cont state

  EffectHandler loc state handler cont -> case fromJSON message of
    Success newState -> do
      pure $ EffectHandler loc newState handler cont

  x -> pure x

applyEvent :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
applyEvent eventBus fromEvent@FromEvent { message, location } component = case component of
  MessageHandler loc state handler cont -> pure $ case fromJSON message of
    Success action' ->
      if loc == location
      then MessageHandler loc (handler action' state) handler cont
      else MessageHandler loc state handler cont
    Error _ ->
      MessageHandler loc state handler cont

  EffectHandler loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      void . forkIO $ do
        newState <-
          if loc == location
          then handler parsedAction state
          else pure state

        atomically $ writeTChan eventBus $ FromEvent
          { event = "newState"
          , message = toJSON newState
          , location = loc
          }
      pure $ EffectHandler loc state handler cont
    Error err ->
      pure $ EffectHandler loc state handler cont

  Html kind children -> do
    children' <- mapM (applyEvent eventBus fromEvent) children
    pure $ Html kind children'

  Hide x -> do
    child <- applyEvent eventBus fromEvent x
    pure $ Hide child

  x -> pure x

{-|

For now the only special event kind is "newState" which replaces
the inner state of a Handler (Message or Effect)

-}

apply :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
apply eventBus fromEvent@FromEvent {event=eventKind} component =
  case eventKind of
    "newState" -> applyNewState eventBus fromEvent component
    _          -> applyEvent eventBus fromEvent component

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

-}

prepareGraph :: Purview a -> (Purview a, [FromEvent])
prepareGraph = prepareGraph' []

type Location = [Int]

prepareGraph' :: Location -> Purview a -> (Purview a, [FromEvent])
prepareGraph' location component = case component of
  Attribute attrs cont ->
    let result = prepareGraph' location cont
    in (Attribute attrs (fst result), snd result)

  Html kind children ->
    let result = fmap (\(index, child) -> prepareGraph' (index:location) child) (zip [0..] children)
    in (Html kind (fmap fst result), concatMap snd result)

  MessageHandler loc state handler cont ->
    let
      rest = fmap (prepareGraph' (0:location)) cont
    in
      (MessageHandler (Just location) state handler (\state -> fst (rest state)), snd (rest state))

  EffectHandler loc state handler cont ->
    let
      rest = fmap (prepareGraph' (0:location)) cont
    in
      (EffectHandler (Just location) state handler (\state -> fst (rest state)), snd (rest state))

  Once effect hasRun cont ->
    let send message =
          FromEvent
            { event = "once"
            , message = toJSON message
            , location = Just location
            }
    in if not hasRun then
        let
          rest = prepareGraph' location cont
        in
          (Once effect True (fst rest), [effect send] <> (snd rest))
       else
        let
          rest = prepareGraph' location cont
        in
          (Once effect True (fst rest), snd rest)

  Hide x ->
    let (child, actions) = prepareGraph' location x
    in (Hide child, actions)

  component -> (component, [])
