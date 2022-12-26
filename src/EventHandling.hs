{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EventHandling where

import           Control.Concurrent.STM.TChan
import           Data.Aeson
import           Data.Typeable
import           Data.Maybe

import           Events
import           Component

import Debug.Trace

{-|

This is a special case event to assign new state to handlers

-}
applyNewState
  :: Event
  -> Purview event m
  -> Purview event m
applyNewState fromEvent@(StateChangeEvent newStateFn location) component = case component of
  EffectHandler ploc loc state handler cont -> case cast newStateFn of
    Just newStateFn' -> EffectHandler ploc loc (newStateFn' state) handler cont
    Nothing ->
      let children = fmap (applyNewState fromEvent) cont
      in EffectHandler ploc loc state handler children

  Html kind children ->
    Html kind $ fmap (applyNewState fromEvent) children

  Attribute n cont ->
    Attribute n (applyNewState fromEvent cont)

  Once fn run cont ->
    Once fn run $ applyNewState fromEvent cont

  Text x -> Text x

  Value x -> Value x
applyNewState (Event {}) component = component


data AnyEvent where
  AnyEvent :: (Show evt, Typeable evt, Eq evt) => evt -> AnyEvent

instance Show AnyEvent where
  show (AnyEvent evt) = show evt

instance Eq AnyEvent where
  (AnyEvent evt) == (AnyEvent evt') = case cast evt' of
    Just evt'' -> evt == evt''
    Nothing    -> False

findEvent :: Event -> Purview event m -> Maybe AnyEvent

findEvent event@Event { message=childLocation, location=handlerLocation } tree = case tree of
  Attribute attr cont -> case attr of
    On _ ident evt ->
      if ident == childLocation
      then Just $ AnyEvent evt
      else Nothing
    _ -> findEvent event cont

  Html _ children ->
    case mapMaybe (findEvent event) children of
      [found] -> Just found
      [] -> Nothing

  EffectHandler _ ident state _ cont ->
    findEvent event (cont state)

  Text _ -> Nothing

  Value _ -> Nothing

{- right, can't do this because my brain isn't big enough -}
-- findInTree ::(Purview event m -> Bool) -> Purview event m -> Maybe (Purview event m)
-- findInTree test tree = case tree of
--   Attribute attr cont ->
--     if test tree
--     then Just tree
--     else findInTree test cont
--
--   Html _ children ->
--     if test tree
--     then Just tree
--     else case mapMaybe (findInTree test) children of
--       [found] -> Just found
--       []      -> Nothing
--
--   EffectHandler _ ident _ _ cont ->
--     let found = fmap (\cont' -> findInTree test cont') cont
--     in undefined
--
--   Text _ -> Nothing
--
--   Value _ -> Nothing
--
-- findParent :: Identifier -> Purview event m -> Purview event m
-- findParent ident tree = undefined
--
-- findChild :: Identifier -> Purview event m -> Maybe event
-- findChild = undefined

-- runEvent :: Monad m => Event -> Purview event m -> m [Event]
-- runEvent (StateChangeEvent _ _) _ = pure []
-- runEvent fromEvent@(Event { message, location }) component = case component of
--   EffectHandler parentLocation loc state handler cont -> case fromJSON message of
--     Success parsedAction -> do
--       -- if locations match, we actually run what is in the handler
--       (newStateFn, events) <-
--         if loc == location
--         then handler parsedAction state
--         else pure (const state, [])
--
--       -- although it doesn't break anything, only send this when the
--       -- locations match (cuts down on noise)
--       let newStateEvent = [StateChangeEvent newStateFn loc | loc == location]
--
--       let createMessage directedEvent = case directedEvent of
--             (Parent event) -> Event
--               -- TODO: this should probably be a new kind of event
--               { event = "internal"
--               , message = toJSON event
--               , location = parentLocation
--               }
--             (Self event) -> Event
--               { event = "internal"
--               , message = toJSON event
--               , location = loc
--               }
--
--       -- here we handle sending events returned to either this
--       -- same handler or passing it up the chain
--       -- mapM_ (atomically . writeTChan eventBus . createMessage) events
--       let handlerEvents = fmap createMessage events
--
--       -- ok, right, no where in this function does the tree actually change
--       -- that's handled by the setting state event
--       childEvents <- runEvent fromEvent (cont state)
--
--       -- so we can ignore the results from applyEvent and continue
--       -- pure $ EffectHandler parentLocation loc state handler cont
--       pure $ newStateEvent <> handlerEvents <> childEvents
--
--     Error _err -> runEvent fromEvent (cont state)
--
--   Html kind children -> do
--     childEvents' <- mapM (runEvent fromEvent) children
--     pure $ concat childEvents'
--
--   Attribute n cont -> runEvent fromEvent cont
--
--   Once _ _ cont -> runEvent fromEvent cont
--
--   Text _ -> pure []
--
--   Value _ -> pure []
