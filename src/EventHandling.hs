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


{-|

This is a special case event to assign new state to handlers

-}
-- TODO: this doesn't go down the tree or match based on handler ID
applyNewState
  :: Event
  -> Purview event m
  -> Purview event m
applyNewState fromEvent@(StateChangeEvent newStateFn location) component = case component of
  EffectHandler initEvents ploc loc state handler cont -> case cast newStateFn of
    Just newStateFn' -> EffectHandler initEvents ploc loc (newStateFn' state) handler cont
    Nothing ->
      let children = fmap (applyNewState fromEvent) cont
      in EffectHandler initEvents ploc loc state handler children

  Handler initEvents ploc loc state handler cont -> case cast newStateFn of
    Just newStateFn' -> Handler initEvents ploc loc (newStateFn' state) handler cont
    Nothing ->
      let children = fmap (applyNewState fromEvent) cont
      in Handler initEvents ploc loc state handler children

  Html kind children ->
    Html kind $ fmap (applyNewState fromEvent) children

  Attribute n cont ->
    Attribute n (applyNewState fromEvent cont)

  Text x -> Text x

  Value x -> Value x
applyNewState (FromFrontendEvent {}) component = component
applyNewState (InternalEvent {}) component = component


findEvent :: Event -> Purview event m -> Maybe Event
findEvent (StateChangeEvent {}) _ = Nothing
findEvent (InternalEvent {}) _ = Nothing
findEvent event@FromFrontendEvent { childLocation=childLocation, location=handlerLocation } tree = case tree of
  Attribute attr cont -> case attr of
    On _ ident evt ->
      if ident == childLocation
      then Just $ InternalEvent evt childLocation handlerLocation
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

-- TODO: continue down the tree
runEvent :: Monad m => Event -> Purview event m -> m [Event]
runEvent internalEvent@InternalEvent { event, handlerId } tree = case tree of
  Attribute attr cont ->
    runEvent internalEvent cont

  Html _ children -> concat <$> mapM (runEvent internalEvent) children

  EffectHandler _ _ ident state handler cont -> case cast event of
    Just event' -> do
      (newStateFn, events) <- handler event' state

      pure [StateChangeEvent newStateFn handlerId]
    Nothing -> pure []

  Handler _ _ ident state handler cont -> case cast event of
    Just event' ->
      let (newStateFn, events) = handler event' state
      in pure [StateChangeEvent newStateFn handlerId]
    Nothing -> pure []

  Text _ -> pure []

  Value _ -> pure []
