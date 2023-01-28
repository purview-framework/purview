{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PrepareTree where

import Data.Typeable

import Component
import Events

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}
prepareTree :: Typeable event => Purview event m -> ([Event], Purview event m)
prepareTree = prepareTree' [] []

type Location = [Int]

addLocationToAttr :: Location -> Attributes e -> Attributes e
addLocationToAttr loc attr = case attr of
  On str _ event' -> On str (Just loc) event'
  _               -> attr

directedEventToInternalEvent :: (Typeable a, Typeable b) => Location -> Location -> DirectedEvent a b -> Event
directedEventToInternalEvent parentLocation location directedEvent = case directedEvent of
  Parent event -> InternalEvent { event=event, childId=Nothing, handlerId=Just parentLocation }
  Self event   -> InternalEvent { event=event, childId=Nothing, handlerId=Just location }

prepareTree' :: Typeable event => Location -> Location -> Purview event m -> ([Event], Purview event m)
prepareTree' parentLocation location component = case component of
  Attribute attr cont ->
    let
      (events, child) = prepareTree' parentLocation (location <> [0]) cont
    in
      (events, Attribute (addLocationToAttr location attr) child)

  Html kind children ->
    let
      indexedChildren = zip [0..] children
      eventsAndChildren =
        fmap (\(location', child) -> prepareTree' parentLocation (location <> [location']) child) indexedChildren
      events = concatMap fst eventsAndChildren
      children' = fmap snd eventsAndChildren
    in
      (events, Html kind children')

  EffectHandler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
      (childEvents, _) = cont' state
    in
      ( fmap (directedEventToInternalEvent parentLocation location) initEvents <> childEvents
      , EffectHandler (Just parentLocation) (Just location) [] state handler (snd . cont')
      )

  Handler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
      (childEvents, _) = cont' state
    in
      ( fmap (directedEventToInternalEvent parentLocation location) initEvents <> childEvents
      , Handler (Just parentLocation) (Just location) [] state handler (snd . cont')
      )

  Text val -> ([], Text val)

  Value val -> ([], Value val)
