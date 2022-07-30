-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
module Experiment21 where
import qualified GHC.Num.BigNat as Html

{-

There has to be a way

3 ideas:
- try the semigroup / monad approach again
- try moving the requirement for parent event to handler
  only and somehow getting it to infer the parent
- type families??

-}

-- state what you want

{-

This is for a sort of web framework I've been working on.

Handlers can send events to themself, or to a parent handler.

Senders (triggered by events in the HTML) can only send events to the
handler above them in the graph.

There will be few handlers / senders, and a lot of Html. Having to
type out the parentEvent for Sender / Html gets weird, since they
don't have to know about it, so I'd like to hide parentEvent from
the user in some way (any way).  It does need to be kept track of,
somehow, since new Handlers can be nested in the graph layers beneath
the parent handler.

I've been wondering if this could be solved with implicit params or
something monadic, but I'm not sure.  Maybe there's an easier way?
Or the data model should be changed?  Any advice appreciated as
I've been stuck on this for awhile and my brain has calcified.

-}

data Event a b = Parent a | Self b

data Graph parentEvent event where
  Sender
    :: event
    -> [Graph parentEvent event]
    -- ^ children
    -> Graph parentEvent event
  Handler ::
    { handler :: newEvent -> [Event parentEvent newEvent]
    -- ^ receives the sender event and generates new events
    , children :: [Graph newEvent any]
    }
    -> Graph parentEvent newEvent
  Html
    :: String
    -> [Graph parentEvent event]
    -- ^ children
    -> Graph parentEvent event

data Vertical = North | South
data Horizontal = East | West

type Graph' a = forall parentEvent. Graph parentEvent a

-- here you can see where having to state the parentEvent type makes less sense
-- and could be annoying to people using the library
westSender :: Graph' Horizontal
westSender = Sender West []

child :: Graph Vertical Horizontal
child = Handler (\event -> [ Parent North, Self East ]) [ westSender ]

parent :: [Graph Vertical any] -> Graph () Vertical
parent = Handler (\event -> [ Self North ])

graph :: Graph () Vertical
graph = parent [ child ]
