{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Experiment19 where

import Data.Proxy

{-

Monoids do _not_ have a law like a <> b == b <> a, I dunno
why I was thinking they did

-- Identity laws
x <> mempty = x
mempty <> x = x

-- Associativity
(x <> y) <> z = x <> (y <> z)

-}

{-

So, can we hide information from the user to track the parent
effect?  What would this look like?

(>>=) :: m a -> (a -> m b) -> m b

(<*>) :: f (a -> b) -> f a -> f b

Maybe all I need is monoid?

-}

-- data Box where
--   Box ::
--     { parentActions :: Proxy a
--     , currentActions :: Proxy b
--     , component :: Carrier
--     } -> Box
--
--
-- data Carrier where
--   Handler
--     :: state
--     -> (action -> state -> state)
--     -> (state -> Box parentAction action)
--     -> Carrier

{-

Maybe I don't need anything, just a separation between
the internal model that tracks actions and the AST.

Fundamentally it's always a continuation, yet that information
is lost.

pure would embed the rest into a context, I guess with the
starting tree? if they aren't already?

wtf is the synthesis between these concepts.  Should the handler
take a type?

-}

-- data Carrier where
--   Handler
--     :: Proxy parentAction
--     -> state
--     -> (action -> state -> state)
--     -> (state -> Box parentAction action)
--     -> Carrier
--   Nil
--     :: a
--     -> Carrier

-- alright it has a proxy, how does construction look?

-- test = Handler ""

{-
src/experiments/Experiment19.hs:75:16-17: error: …
    • Couldn't match type: [Char]
                     with: Proxy parentAction
      Expected: Proxy parentAction
        Actual: String

so, nope, wrong approach.  it's to expose a type variable
not hide one, basically the opposite of what I want to do

-}


{-

Back to the semigroup idea

-}

-- data Box where
--   Box ::
--     { currentActions :: Proxy b
--     , component :: Carrier
--     } -> Box
--
-- instance Semigroup Box where
--   -- so it replaces the actions allowed
--   Box current component <> Box current' component' = Box current' component'
--
-- -- so for contruction...
--
-- test = Box (Proxy :: Proxy Int) Nil
--
--
-- data Carrier where
--   Handler
--     :: state
--     -> (action -> state -> state)
--     -> (state -> Box)
--     -> Carrier
--   Nil :: Carrier

{-

Ok interesting.  Think dumber.  Let's construct a more basic
form of what's wanted.

-}

-- With this there are no constraints on children
-- data Graph where
--   Node :: a -> Graph

-- data Graph action where
--   Node :: s -> (Graph a) -> Graph a

-- nope you went too complex again

data Graph where
  Node :: (?parentAction :: parentAction) => a -> Graph
