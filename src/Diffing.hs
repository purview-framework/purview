{-# LANGUAGE DeriveGeneric #-}
module Diffing where

import GHC.Generics
import Data.Aeson

import Component
import Unsafe.Coerce (unsafeCoerce)

{-

Since events target specific locations, we can't stop going the tree early
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

diff
  :: Maybe Location
  -> Location
  -> Purview event m
  -> Purview event m
  -> [Change (Purview event m)]
diff target location oldGraph newGraph = case (oldGraph, newGraph) of

  (Html kind children, Html kind' children') ->
    concatMap
      (\(index, oldChild, newChild) -> diff target (index:location) oldChild newChild)
      (zip3 [0..] children children')

  (Text str, Text str') ->
    [Update location (Text str') | str /= str']

  (Html kind children, unknown) ->
    [Update location newGraph]

  (unknown, Html kind children) ->
    [Update location newGraph]

  -- TODO: add Handler
  (EffectHandler _ loc initEvents state _ cont, EffectHandler _ loc' initEvents' newState _ newCont) ->
    [Update location newGraph | unsafeCoerce state /= newState && loc == loc']
      -- TODO: this is weak, instead of walking the whole tree it should be targetted
      --       to specific effect handlers

      -- if we hit the target, we're already saying update the whole tree
      <> if Just location == target
         then []
         else diff target (0:location) (unsafeCoerce cont state) (unsafeCoerce newCont newState)


  (Attribute attr a, Attribute attr' b) ->
    [Update location newGraph | attr /= attr']

  (Value _, _) ->
    [Update location newGraph]

  (EffectHandler _ _ _ _ _ _, _) ->
    [Update location newGraph]

  (_, _) -> [Update location newGraph]

  -- (a, b) -> error (show a <> "\n" <> show b)
