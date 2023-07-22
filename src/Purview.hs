{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|

Purview aims to be pretty straightforward to work with.  As an example,
here's a counter that we'll then go through.

> module Main where
>
> import Purview
>
> incrementButton = onClick "increment" $ button [ text "+" ]
> decrementButton = onClick "decrement" $ button [ text "-" ]
>
> view count = div
>   [ p [ text ("count: " <> show count) ]
>   , incrementButton
>   , decrementButton
>   ]
>
> handler :: (Integer -> Purview String any IO) -> Purview () any IO
> handler = simpleHandler (0 :: Integer) reducer
>
> reducer action state = case action of
>   "increment" -> state + 1
>   "decrement" -> state - 1
>
> top = handler view
>
> main = run defaultConfiguration { component=top, devMode=True }

First we define two buttons, each which have action producers ('onClick').

When rendered, this tells Purview that when either is clicked it'd like to receive
a message ('increment' or 'decrement').

Then we define a handler, which takes an initial state ("0"), and a reducer.

The reducer defines how we're supposed to handle the events received, and it passes
down the new state to components.

Then we put it together ("handler view"), and run it.

Note the "devMode=True": this tells Purview to send the whole
tree over again when the websocket reconnects.  This is really handy
if you're re-running the server in ghci, although I really recommend
using ghcid so you can do:

> ghcid --command 'stack ghci yourProject/Main.hs' --test :main

Which will automatically restart the server on code changes.  It's fast!

For more in depth reading check out the [readme](https://github.com/purview-framework/purview/blob/main/README.md) and
the [examples](https://github.com/purview-framework/purview/tree/main/examples) folder.

-}

module Purview
  (
  -- ** Server
  Configuration (..)
  , defaultConfiguration
  , renderFullPage
  , startWebSocketLoop

  -- ** Handlers
  -- | These are how you can catch events sent from things like 'onClick' and
  -- change state, or in the case of 'effectHandler', make API requests or call
  -- functions from your project.
  , handler
  , handler'
  , effectHandler
  , effectHandler'
  , receiver

  -- ** QuasiQuoter for styling
  , style

  -- ** HTML helpers
  , div
  , span
  , p
  , h1
  , h2
  , h3
  , h4
  , text
  , button
  , form
  , input
  , istyle
  , id'
  , class'

  -- ** Action producers
  , onClick
  , onSubmit
  , onBlur
  , onChange

  -- ** For Testing
  , render

  -- ** AST
  , Attributes (..)
  , DirectedEvent (..)
  , Purview (..)
  )
where

import           Prelude hiding (div, log, span)

import           Style (style)
import           Component (Purview (Attribute), Attributes(..))
import           ComponentHelpers
import           Events
import           Rendering
import           Configuration
import           Server


defaultConfiguration :: Configuration IO
defaultConfiguration = Configuration
  { interpreter       = id
  , logger            = print
  , eventsToListenTo  = [ "click", "focusout", "focusin", "change", "submit" ]
  , htmlHead          = ""
  , devMode           = False
  , eventProducers    = []
  , eventListeners    = []
  }

