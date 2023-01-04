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
  EffectHandler ploc loc initEvents state handler cont -> case cast newStateFn of
    Just newStateFn' -> EffectHandler ploc loc initEvents (newStateFn' state) handler cont
    Nothing ->
      let children = fmap (applyNewState fromEvent) cont
      in EffectHandler ploc loc initEvents state handler children

  Handler ploc loc initEvents state handler cont -> case cast newStateFn of
    Just newStateFn' -> Handler ploc loc initEvents (newStateFn' state) handler cont
    Nothing ->
      let children = fmap (applyNewState fromEvent) cont
      in Handler ploc loc initEvents state handler children

  Html kind children ->
    Html kind $ fmap (applyNewState fromEvent) children

  Attribute n cont ->
    Attribute n (applyNewState fromEvent cont)

  Text x -> Text x

  Value x -> Value x
applyNewState (Event {}) component = component


findEvent :: Event -> Purview event m -> Maybe AnyEvent
findEvent (StateChangeEvent _ _) _ = Nothing
findEvent event@Event { message=childLocation, location=handlerLocation } tree = case tree of
  Attribute attr cont -> case attr of
    On _ ident evt ->
      if ident == childLocation
      then Just $ AnyEvent evt childLocation handlerLocation
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
runEvent :: Monad m => AnyEvent -> Purview event m -> m [Event]
runEvent anyEvent@AnyEvent { event, handlerId } tree = case tree of
  Attribute attr cont ->
    runEvent anyEvent cont

  Html _ children -> concat <$> mapM (runEvent anyEvent) children

  EffectHandler _ ident initEvents state handler cont -> case cast event of
    Just event' -> do
      (newStateFn, events) <- handler event' state

      pure [StateChangeEvent newStateFn handlerId]
    Nothing -> pure []

  Handler _ ident initEvents state handler cont -> case cast event of
    Just event' ->
      let (newStateFn, events) = handler event' state
      in pure [StateChangeEvent newStateFn handlerId]
    Nothing -> pure []

  Text _ -> pure []

  Value _ -> pure []
