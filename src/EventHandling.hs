{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EventHandling where

import           Control.Concurrent.STM.TChan
import           Data.Aeson

import Events
import Component

{-|

This is a special case event to assign state to message handlers

-}

applyNewState :: TChan FromEvent -> FromEvent -> Purview a m -> Purview a m
applyNewState eventBus fromEvent@FromEvent { message, location } component = case component of
  EffectHandler ploc loc state handler cont -> case fromJSON message of
    Success newState -> do
      if loc == location
        then EffectHandler ploc loc newState handler cont
        -- TODO: continue down the tree
        else EffectHandler ploc loc state handler cont
    Error _ -> do
      EffectHandler ploc loc state handler cont

  Hide x ->
    let
      children = applyNewState eventBus fromEvent x
    in
      Hide children

  -- TODO: continue down the tree
  x -> x

runEvent :: Monad m => FromEvent -> Purview a m -> m [FromEvent]
runEvent fromEvent@FromEvent { message, location } component = case component of
  EffectHandler parentLocation loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      -- if locations match, we actually run what is in the handler
      (newState, events) <-
        if loc == location
        then handler parsedAction state
        else pure (state, [])

      -- although it doesn't break anything, only send this when the
      -- locations match (cuts down on noise)
      let newStateEvent =
            if loc == location then
              [
                FromEvent
                { event = "newState"
                , message = toJSON newState
                , location = loc
                }
              ]
            else
              []

      let createMessage directedEvent = case directedEvent of
            (Parent event) -> FromEvent
              { event = "internal"
              , message = toJSON event
              , location = parentLocation
              }
            (Self event) -> FromEvent
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

  Hide x -> runEvent fromEvent x

  x -> pure []

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}

prepareGraph :: Purview a m -> (Purview a m, [FromEvent])
prepareGraph = prepareGraph' [] []

type Location = [Int]

prepareGraph' :: Location -> Location -> Purview a m -> (Purview a m, [FromEvent])
prepareGraph' parentLocation location component = case component of
  Attribute attrs cont ->
    let result = prepareGraph' parentLocation location cont
    in (Attribute attrs (fst result), snd result)

  Html kind children ->
    let result = fmap (\(index, child) -> prepareGraph' parentLocation (index:location) child) (zip [0..] children)
    in (Html kind (fmap fst result), concatMap snd result)

  EffectHandler _ploc _loc state handler cont ->
    let
      rest = fmap (prepareGraph' location (0:location)) cont
    in
      ( EffectHandler (Just parentLocation) (Just location) state handler (\state' -> fst (rest state'))
      , snd (rest state)
      )

  Once effect hasRun cont ->
    let send message =
          FromEvent
            { event = "once"
            , message = toJSON message
            , location = Just location
            }
    in if not hasRun then
        let
          rest = prepareGraph' parentLocation location cont
        in
          (Once effect True (fst rest), [effect send] <> (snd rest))
       else
        let
          rest = prepareGraph' parentLocation location cont
        in
          (Once effect True (fst rest), snd rest)

  Hide x ->
    let (child, actions) = prepareGraph' parentLocation location x
    in (Hide child, actions)

  component' -> (component', [])
