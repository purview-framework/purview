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
parentEvent on Senders and Html doesn't fit the model, since it's
not needed.  Is there a better way to model this that would remove
parentEvent, but still keep track of it?  New Handlers can be anywhere
in the tree and need to know what events it can send to the parent.

If there was some way for the Handler to "know" that the "action" type
of the Sender / Html it's embedded in was its "parentEvent" type, that'd
be pretty cool but might not be possible?

I've been wondering if this could be solved with implicit params or
something monadic, but I'm not sure.

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

data RegistrationFormEvent = UserInfoSubmitted { email :: Maybe String, username :: Maybe String }

registrationForm = Sender UserInfoSubmitted
  [ Html "form"
      [ -- email input
        -- username input
        -- submit button
      ]
  ]

registrationHandler = Handler handle
  where handle UserInfoSubmitted { name, username } = do
          -- check the name is valid
          -- check the username is valid
          --

-- here you can see where having the parentEvent type makes less sense
westSender :: Graph parentEvent Horizontal
westSender = Sender West [ Html "button" [] ]

child :: Graph Vertical Horizontal
child = Handler (\event -> [ Parent North, Self East ]) [ westSender ]

parent :: [Graph Vertical any] -> Graph () Vertical
parent = Handler (\event -> [ Self North ])

graph :: Graph () Vertical
graph = parent [ child ]

-- I see I can do something like this:
type Sender event = forall parentEvent. Graph parentEvent event

-- to hide having to type out the parentEvent, but it's still not
-- exactly matching with how it really works
eastSender :: Sender Horizontal
eastSender = Sender East []
