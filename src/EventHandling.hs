{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EventHandling where

import           Control.Concurrent.STM.TChan
import           Data.Aeson
import           Data.Typeable

import           Events
import           Component


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


runEvent :: Monad m => Event -> Purview event m -> m [Event]
runEvent (StateChangeEvent _ _) _ = pure []
runEvent fromEvent@(Event { message, location }) component = case component of
  EffectHandler parentLocation loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      -- if locations match, we actually run what is in the handler
      (newStateFn, events) <-
        if loc == location
        then handler parsedAction state
        else pure (const state, [])

      -- although it doesn't break anything, only send this when the
      -- locations match (cuts down on noise)
      let newStateEvent = [StateChangeEvent newStateFn loc | loc == location]

      let createMessage directedEvent = case directedEvent of
            (Parent event) -> Event
              -- TODO: this should probably be a new kind of event
              { event = "internal"
              , message = toJSON event
              , location = parentLocation
              }
            (Self event) -> Event
              { event = "internal"
              , message = toJSON event
              , location = loc
              }

      -- here we handle sending events returned to either this
      -- same handler or passing it up the chain
      -- mapM_ (atomically . writeTChan eventBus . createMessage) events
      let handlerEvents = fmap createMessage events

      -- ok, right, no where in this function does the tree actually change
      -- that's handled by the setting state event
      childEvents <- runEvent fromEvent (cont state)

      -- so we can ignore the results from applyEvent and continue
      -- pure $ EffectHandler parentLocation loc state handler cont
      pure $ newStateEvent <> handlerEvents <> childEvents

    Error _err -> runEvent fromEvent (cont state)

  Html kind children -> do
    childEvents' <- mapM (runEvent fromEvent) children
    pure $ concat childEvents'

  Attribute n cont -> runEvent fromEvent cont

  Once _ _ cont -> runEvent fromEvent cont

  Text _ -> pure []

  Value _ -> pure []
