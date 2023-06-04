{-# LANGUAGE NamedFieldPuns #-}
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
prepareTree :: Typeable event => Purview event m -> ([Event], [(Hash, String)], Purview event m)
prepareTree = prepareTree' [] []

type Location = [Int]

addLocationToAttr :: Location -> Attributes e -> Attributes e
addLocationToAttr loc attr = case attr of
  On str _ event' -> On str (Just loc) event'
  _               -> attr

getStyleFromAttr :: Attributes e -> (Maybe (Hash, String), Attributes e)
getStyleFromAttr attr = case attr of
  Style (hash, css) ->
    if hash /= "-1"
    then (Just (hash, css), Style (hash, ""))  -- set the css to empty since it's been caught
    else (Nothing, attr)
  _ -> (Nothing, attr)

directedEventToInternalEvent :: (Typeable a, Typeable b) => Location -> Location -> DirectedEvent a b -> Event
directedEventToInternalEvent parentLocation location directedEvent = case directedEvent of
  Parent event -> InternalEvent { event=event, childId=Nothing, handlerId=Just parentLocation }
  Self event   -> InternalEvent { event=event, childId=Nothing, handlerId=Just location }
  Browser {}   -> error "tried to turn a browser event into an internal event"

prepareTree'
  :: Typeable event
  => Location
  -> Location
  -> Purview event m
  -> ([Event], [(Hash, String)], Purview event m)
prepareTree' parentLocation location component = case component of
  Attribute attr cont ->
    let
      (events, css, child) = prepareTree' parentLocation (location <> [0]) cont
      (possibleCss, newAttr) = getStyleFromAttr $ addLocationToAttr location attr
    in
      case possibleCss of
        Just newCss -> (events, newCss : css, Attribute (addLocationToAttr location attr) child)
        Nothing     -> (events, css, Attribute newAttr child)

  Html kind children ->
    let
      indexedChildren = zip [0..] children
      eventsAndChildren =
        fmap (\(location', child) -> prepareTree' parentLocation (location <> [location']) child) indexedChildren
      events = concatMap (\(it, _, _) -> it) eventsAndChildren
      css = concatMap (\(_, it, _) -> it) eventsAndChildren
      children' = fmap (\(_, _, it) -> it) eventsAndChildren
    in
      (events, css, Html kind children')

  EffectHandler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
      (childEvents, css, _) = cont' state
    in
      ( fmap (directedEventToInternalEvent parentLocation location) initEvents <> childEvents
      , css
      , EffectHandler (Just parentLocation) (Just location) [] state handler ((\(_, _, it) -> it) . cont')
      )

  Handler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
      (childEvents, css, _) = cont' state
    in
      ( fmap (directedEventToInternalEvent parentLocation location) initEvents <> childEvents
      , css
      , Handler (Just parentLocation) (Just location) [] state handler ((\(_, _, it) -> it) . cont')
      )

  Receiver { name, eventHandler } ->
    ([], [], Receiver (Just parentLocation) (Just location) name eventHandler)

  Text val -> ([], [], Text val)

  Value val -> ([], [], Value val)
