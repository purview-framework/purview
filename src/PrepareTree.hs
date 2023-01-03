{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PrepareTree where

import Component
import Events

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}

prepareTree :: Purview event m -> (Purview event m, [Event])
prepareTree = prepareTree' [] []

type Location = [Int]

addLocationToAttr :: Location -> (Attributes e) -> (Attributes e)
addLocationToAttr loc attr = case attr of
  On str _ event' -> On str (Just loc) event'
  _ -> attr

addLocations :: Purview event m -> Purview event m
addLocations = addLocations' [] []

{- This one we'll do full paths to items? -}
addLocations' :: Location -> Location -> Purview event m -> Purview event m
addLocations' parentLocation location component = case component of
  Attribute attr cont ->
    let
      child = addLocations' parentLocation (location <> [0]) cont
    in
      Attribute (addLocationToAttr location attr) child

  Html kind children ->
    let
      indexedChildren = zip [0..] children
      children' =
        fmap (\(location', child) -> addLocations' parentLocation (location <> [location']) child) indexedChildren
    in
      Html kind children'

  EffectHandler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (\child -> addLocations' location (location <> [0]) child) cont
    in
      EffectHandler (Just parentLocation) (Just location) initEvents state handler cont'

  Handler _ploc _loc initEvents state handler cont ->
    let
      cont' = fmap (\child -> addLocations' location (location <> [0]) child) cont
    in
      Handler (Just parentLocation) (Just location) initEvents state handler cont'

  Text val -> Text val

  Value val -> Value val


prepareTree'
  :: Location
  -> Location
  -> Purview event m
  -> (Purview event m, [Event])
prepareTree' parentLocation location component = case component of
  Attribute attr cont ->
    let
      result = prepareTree' parentLocation location cont
      newAttr = addLocationToAttr location attr
    in
      (Attribute newAttr (fst result), snd result)

  Html kind children ->
    let result = fmap (\(index, child) -> prepareTree' parentLocation (index:location) child) (zip [0..] children)
    in (Html kind (fmap fst result), concatMap snd result)

  EffectHandler _ploc _loc initEvents state handler cont ->
    let
      rest = fmap (prepareTree' location (0:location)) cont
    in
      ( EffectHandler (Just parentLocation) (Just location) initEvents state handler (\state' -> fst (rest state'))
      , snd (rest state)
      )

  Handler _ploc _loc initEvents state handler cont ->
    let
      rest = fmap (prepareTree' location (0:location)) cont
    in
      ( Handler (Just parentLocation) (Just location) initEvents state handler (\state' -> fst (rest state'))
      , snd (rest state)
      )

  Value x -> (Value x, [])

  Text x -> (Text x, [])
