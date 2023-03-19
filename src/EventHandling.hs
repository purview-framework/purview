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


type Location = [Int]

directedEventToInternalEvent :: (Typeable a, Typeable b) => Location -> Location -> DirectedEvent a b -> Event
directedEventToInternalEvent parentLocation location directedEvent = case directedEvent of
  Parent event -> InternalEvent { event=event, childId=Nothing, handlerId=Just parentLocation }
  Self event   -> InternalEvent { event=event, childId=Nothing, handlerId=Just location }

{-|

This is a special case event to assign new state to handlers

-}
applyNewState
  :: Event
  -> Purview event m
  -> Purview event m
applyNewState fromEvent@(StateChangeEvent newStateFn location) component = case component of
  EffectHandler ploc loc initEvents state handler cont ->
    if loc == location then
      case cast newStateFn of
        Just newStateFn' -> EffectHandler ploc loc initEvents (newStateFn' state) handler cont
        Nothing ->
          let children = fmap (applyNewState fromEvent) cont
          in EffectHandler ploc loc initEvents state handler children
    else
      let children = fmap (applyNewState fromEvent) cont
      in EffectHandler ploc loc initEvents state handler children

  Handler ploc loc initEvents state handler cont ->
    if loc == location then
      case cast newStateFn of
        Just newStateFn' -> Handler ploc loc initEvents (newStateFn' state) handler cont
        Nothing ->
          let children = fmap (applyNewState fromEvent) cont
          in Handler ploc loc initEvents state handler children
    else
      let children = fmap (applyNewState fromEvent) cont
      in Handler ploc loc initEvents state handler children

  Html kind children ->
    Html kind $ fmap (applyNewState fromEvent) children

  Attribute n cont ->
    Attribute n (applyNewState fromEvent cont)

  Text x -> Text x

  Value x -> Value x
applyNewState (FromFrontendEvent {}) component = component
applyNewState (InternalEvent {}) component = component


findEvent :: Event -> Purview event m -> Maybe Event
findEvent StateChangeEvent {} _ = Nothing
findEvent InternalEvent {} _ = Nothing
findEvent event@FromFrontendEvent { childLocation=childLocation, location=handlerLocation, value=value } tree = case tree of
  Attribute attr cont -> case attr of
    On _ ident evt ->
      if ident == childLocation
      then Just $ InternalEvent (evt value) childLocation handlerLocation
      else Nothing
    _ -> findEvent event cont

  Html _ children ->
    case mapMaybe (findEvent event) children of
      [found] -> Just found
      []      -> Nothing
      _       -> Nothing

  EffectHandler _ ident initEvents state _ cont ->
    findEvent event (cont state)

  Handler _ ident initEvents state _ cont ->
    findEvent event (cont state)

  Text _ -> Nothing

  Value _ -> Nothing

runEvent :: (Typeable event, Monad m) => Event -> Purview event m -> m [Event]
runEvent FromFrontendEvent {} _ = pure []
runEvent StateChangeEvent {}  _ = pure []
runEvent internalEvent@InternalEvent { event, handlerId } tree = case tree of
  Attribute attr cont ->
    runEvent internalEvent cont

  Html _ children -> concat <$> mapM (runEvent internalEvent) children

  EffectHandler (Just parentIdent) (Just ident) initEvents state handler cont ->
    if Just ident == handlerId then
      case cast event of
        Just event' -> do
          (newStateFn, events) <- handler event' state
          pure $ [StateChangeEvent newStateFn handlerId] <> fmap (directedEventToInternalEvent parentIdent ident) events
        Nothing -> pure []
    else
      runEvent internalEvent (cont state)

  Handler (Just parentIdent) (Just ident) initEvents state handler cont ->
    if Just ident == handlerId then
      case cast event of
        Just event' ->
          let (newStateFn, events) = handler event' state
          in pure $ [StateChangeEvent newStateFn handlerId] <> fmap (directedEventToInternalEvent parentIdent ident) events
        Nothing -> pure []
    else
      runEvent internalEvent (cont state)

  Text _ -> pure []

  Value _ -> pure []

  -- TODO: this should never happen, should refactor so it's clear
  EffectHandler { identifier = Nothing }       -> undefined
  EffectHandler { parentIdentifier = Nothing } -> undefined
  Handler { identifier = Nothing }             -> undefined
  Handler { parentIdentifier = Nothing }       -> undefined
