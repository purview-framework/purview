{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PrepareTree where

import Data.Typeable

import Component




{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}
prepareTree :: Typeable event => Purview event m -> Purview event m
prepareTree = prepareTree' [] []

type Location = [Int]

addLocationToAttr :: Location -> Attributes e -> Attributes e
addLocationToAttr loc attr = case attr of
  On str _ event' -> On str (Just loc) event'
  _               -> attr

prepareTree'
  :: Typeable event
  => Location
  -> Location
  -> Purview event m
  -> Purview event m
prepareTree' parentLocation location component = case component of
  Attribute attr cont ->
    let
      child = prepareTree' parentLocation (location <> [0]) cont
      newAttr = addLocationToAttr location attr
    in
      Attribute newAttr child

  Html kind children ->
    let
      indexedChildren = zip [0..] children
      children' =
        fmap (\(location', child) -> prepareTree' parentLocation (location <> [location']) child) indexedChildren
    in
      Html kind children'

  EffectHandler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
    in
      EffectHandler (Just parentLocation) (Just location) initEvents state handler cont'

  Handler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (prepareTree' location (location <> [0])) cont
    in
      Handler (Just parentLocation) (Just location) initEvents state handler cont'

  Receiver { name, eventHandler, child, state } ->
    let
      child' = fmap (prepareTree' location (location <> [0])) child
    in
      Receiver (Just parentLocation) (Just location) name eventHandler child' state

  Text val -> Text val

  Value val -> Value val
