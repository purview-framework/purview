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

This is a special case event to assign state to message handlers

-}

applyNewState :: StateChangeEvent -> Purview parentAction action m -> Purview parentAction action m
applyNewState fromEvent@(StateChangeEvent newStateFn location) component = case component of
  EffectHandler ploc loc state handler cont -> case cast newStateFn of
      Just newStateFn' -> EffectHandler ploc loc (newStateFn' state) handler cont
--    case fromJSON message of
--    Success newState -> do
--      if loc == location
--        then EffectHandler ploc loc newState handler cont
--        else
--          let cont' = fmap (applyNewState fromEvent) cont
--          in EffectHandler ploc loc state handler cont'
--    Error _ ->
--      let cont' = fmap (applyNewState fromEvent) cont
--      in EffectHandler ploc loc state handler cont'

  Hide x ->
    let
      children = applyNewState fromEvent x
    in
      Hide children

  Html kind children ->
    Html kind $ fmap (applyNewState fromEvent) children

  Attribute n cont ->
    Attribute n (applyNewState fromEvent cont)

  Once fn run cont ->
    Once fn run $ applyNewState fromEvent cont

  Text x -> Text x

  Value x -> Value x


runEvent :: Monad m => Either FromEvent StateChangeEvent -> Purview parentAction action m -> m [Either FromEvent StateChangeEvent]
runEvent (Right (StateChangeEvent _ _)) _ = pure []
runEvent fromEvent@(Left (FromEvent { message, location })) component = case component of
  EffectHandler parentLocation loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      -- if locations match, we actually run what is in the handler
      (newStateFn, events) <-
        if loc == location
        then handler parsedAction state
        else pure (const state, [])

      -- although it doesn't break anything, only send this when the
      -- locations match (cuts down on noise)
      let test = StateChangeEvent newStateFn Nothing

      let newStateEvent =
            if loc == location then
              [
                Right $ StateChangeEvent newStateFn loc
--                { event = "newState"
--                -- TODO: this should be happening in the event loop
--                , message = toJSON (newStateFn state)
--                , location = loc
--                }
              ]
            else
              []

      let createMessage directedEvent = Left $ case directedEvent of
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

  Once _ _ cont -> runEvent fromEvent cont

  Text _ -> pure []

  Value _ -> pure []
