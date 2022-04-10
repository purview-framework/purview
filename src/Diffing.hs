{-# LANGUAGE DeriveGeneric #-}
module Diffing where

import GHC.Generics
import Data.Typeable
import Data.Aeson

import Component

{-

Since actions target specific locations, we can't stop going the tree early
because changes may have happened beneath the top level.  kind of the
downside not having a single, passed down, state.

We still need render, but render needs to be targeted to specific locations.

I dunno how it should work lol.

Let's start at the basics, with dumb tests.  If there's a div in the new
tree, and not one in the old tree, it should produce something saying
to add that div.

To know where to make a change, I guess you need a location and a command.

-}
type Location = [Int]

data Change a = Update Location a | Delete Location a | Add Location a
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Change a) where
  toEncoding = genericToEncoding defaultOptions

diff :: Location -> Purview a m -> Purview a m -> [Change (Purview a m)]
diff location oldGraph newGraph = case (oldGraph, newGraph) of

  (Html kind children, Html kind' children') ->
    concatMap
      (\(index, oldChild, newChild) -> diff (index:location) oldChild newChild)
      (zip3 [0..] children children')

  (Text str, Text str') ->
    [Update location (Text str') | str /= str']

  (Html kind children, unknown) ->
    [Update location newGraph]

  (unknown, Html kind children) ->
    [Update location newGraph]

  (Hide (EffectHandler _ _ state _ cont), Hide (EffectHandler _ _ newState _ newCont)) ->
    case cast state of
      Just state' ->
        [Update location newGraph | state' /= newState]
      -- different kinds of state
      Nothing ->
        [Update location newGraph]

  (a, b) -> error (show a <> "\n" <> show b)
