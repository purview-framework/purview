{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PrepareTree where

import Data.Aeson

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

prepareTree'
  :: Location
  -> Location
  -> Purview event m
  -> (Purview event m, [Event])
prepareTree' parentLocation location component = case component of
  Attribute attrs cont ->
    let result = prepareTree' parentLocation location cont
    in (Attribute attrs (fst result), snd result)

  Html kind children ->
    let result = fmap (\(index, child) -> prepareTree' parentLocation (index:location) child) (zip [0..] children)
    in (Html kind (fmap fst result), concatMap snd result)

  EffectHandler _ploc _loc state handler cont ->
    let
      rest = fmap (prepareTree' location (0:location)) cont
    in
      ( EffectHandler (Just parentLocation) (Just location) state handler (\state' -> fst (rest state'))
      , snd (rest state)
      )

  Once effect hasRun cont ->
    let send message =
          Event
            { event = "once"
            , message = toJSON message
            , location = Just location
            }
    in if not hasRun then
        let
          rest = prepareTree' parentLocation location cont
        in
          (Once effect True (fst rest), [effect send] <> (snd rest))
       else
        let
          rest = prepareTree' parentLocation location cont
        in
          (Once effect True (fst rest), snd rest)

  Value x -> (Value x, [])

  Text x -> (Text x, [])
