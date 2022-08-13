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

Handlers can send events to themselves, or to a parent handler.

Senders (triggered by events in the HTML) can only send events to the
handler above them in the graph.

There will be few handlers / senders, and a lot of Html. Having the
event on Senders and Html doesn't fit the model, since it's
not needed.  Is there a better way to model this that would remove
event, but still keep track of it?  New Handlers can be anywhere
in the tree and need to know what events it can send to the parent.

If there was some way for the Handler to "know" that the "action" type
of the Sender / Html it's embedded in was its "event" type, that'd
be pretty cool but might not be possible?

I've been wondering if this could be solved with implicit params or
something monadic, but I'm not sure.

-}

data Event a b = Parent a | Self b

type Reducer event newEvent = Graph event

data Graph event where
  Sender
    :: event
    -> [Graph event]
    -- ^ children
    -> Graph event
  Handler ::
    { handler :: newEvent -> [Event event newEvent]
    -- ^ receives the sender event and generates new events
    , children :: [Graph newEvent]
    }
    -> Reducer event newEvent
  Html
    :: String
    -> [Graph event]
    -- ^ children
    -> Graph event

data Vertical = North | South
data Horizontal = East | West

-- here you can see where having the event type makes less sense
-- westSender :: Graph event
westSender = Sender East [ Html "button" [] ]

-- child :: Graph Vertical
child = Handler (\event -> [ Parent North, Self East ]) [ westSender ]

parent = Handler (\event -> [ Self North ])

graph = parent [ child ]

data Temp where
  Temp :: { value :: a, value2 :: a } -> Temp

test = Temp () ""

{- Checks

- handler / handler mismatch
- sender / handler mismatch

-}


-- -- I see I can do something like this:
-- type Sender event = forall event. Graph event event
--
-- -- to hide having to type out the event, but it's still not
-- -- exactly matching with how it really works
-- eastSender :: Sender Horizontal
-- eastSender = Sender East []
