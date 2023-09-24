{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|

Purview follows the usual pattern of action -> state -> state, with
events flowing up from event producers to handlers where they are
captured.  State is passed from handler to the continuation.

Here's a quick example with a counter:

> module Main where
> import Prelude hiding (div)
> import Purview
> import Purview.Server (serve, defaultConfiguration)
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
> countHandler :: (Integer -> Purview String m) -> Purview () m
> countHandler = handler' [] (0 :: Integer) reducer
>
> reducer event state = case event of
>   "increment" -> (state + 1, [])
>   "decrement" -> (state - 1, [])
>
> component' _ = countHandler view
>
> main = serve defaultConfiguration { devMode=True } component'

Note the "devMode=True": this tells Purview to send the whole
tree over again when the websocket reconnects.  This is handy
if you're re-running the server in ghci, although I recommend
using ghcid so you can do:

> ghcid --command 'stack ghci yourProject/Main.hs' --test :main

Which will automatically restart the server on code changes.  It's fast!

For more in depth reading check out the [readme](https://github.com/purview-framework/purview/blob/main/README.md) and
the [examples](https://github.com/purview-framework/purview/tree/main/examples) folder.

-}

module Purview
  -- ** Handlers
  {-|
These are how you can catch events sent from things like 'onClick' and
change state, or in the case of 'effectHandler', make API requests or call
functions from your project.

In addition they can send events to themself, to a parent, call a
function in the browser, or all three.

Note because of the typeable constraints Haskell will yell at you until
it knows the types of the event and state.
  -}
  ( handler
  , effectHandler
  , handler'
  , effectHandler'

  -- ** Styling
  , style
  , istyle

  -- ** HTML
  {-|
These are some of the more common HTML nodes and some attributes to get you
started, but you'll want to create your own as well.  Here's how:

__Examples:__

If you wanted to create a <code> node:

@
import Purview ( Purview( Html ), text )

code :: [Purview event m] -> Purview event m
code = Html "code"

helloCode :: Purview event m
helloCode = code [ text "it's some code" ]
-- renders as <code>it's some code</code>
@

If you wanted to create a new attribute for adding test-ids to nodes:

@
import Purview ( Purview( Attribute ), Attributes( Generic ), button, text )

testId :: String -> Purview event m -> Purview event m
testId = Attribute . Generic "test-id"

testableButton :: Purview event m
testableButton = testId "cool-button" $ button [ text "testable!" ]
-- renders as <button test-id="cool-button">testable!</button>
@
  -}
  , div
  , span
  , p
  , h1
  , h2
  , h3
  , h4
  , text
  , button
  , a
  , ul
  , li
  , form
  , input
  , href
  , id'
  , class'

  -- ** Events
  {-|
Event creators work similar to attributes in that they are bound to
the eventual concrete HTML.  When triggered they create an event
that flows up to a handler.  They can have a value, in which case
you'll need to provide a function to transform that value into
an event your handler can handle.

To create your own:

__Examples:__

To add an event creator for keydown:

@
import Purview ( Purview( Attribute ), Attributes( On ) )
import Data.Typeable

onKeyDown
  :: ( Typeable event
     , Eq event
     , Show event
     )
  => (Maybe String -> event) -> Purview event m -> Purview event m
onKeyDown = Attribute . On "keydown" Nothing
@

In addition to this, you'll need to add "keydown" to the list of events
listened for in the configuration at the top like so:

@
import Purview.Server (defaultConfiguration, serve, Configuration( eventsToListenTo ))

newConfig =
  let events = eventsToListenTo defaultConfiguration
  in defaultConfiguration { eventsToListenTo="keydown":events }

main = serve newConfig $ const $ div []
@

This is hopefully short-lived and going away in a coming version.
  -}
  , onClick
  , onSubmit
  , onBlur
  , onChange

  -- ** Interop
  {-|
While the receiver covers receiving events, here's how you can call javascript
functions:

__Example:__

Here whenever "increment" is received by the handler, it produces a new
Browser event.  This calls window.addMessage in the browser, with an
argument of the "show newState" -- so, a String.

@
countHandler = handler' [] (0 :: Int) reducer
  where
    reducer "increment" state =
      let newState = state + 1
      -- this being the important bit, you can call any function in javascript
      -- with the Brower fnName value event.
      in (newState, [Browser "addMessage" (show newState)])

jsMessageAdder = [r|
  const addMessage = (value) => {
    const messagesBlock = document.querySelector("#messages");
    messagesBlock.innerHTML = value;
  }
  -- important, otherwise it won't be able to find the function
  window.addMessage = addMessage;
|]

main = serve (defaultConfiguration { javascript=jsMessageAdder }
@

  -}
  , receiver

  -- ** Testing
  , render

  -- ** AST
  , Attributes (..)
  , DirectedEvent (..)
  , Purview (..)
  )
where

import           Prelude hiding (div, log, span)

import           Style (style)
import           Component (Purview (Attribute, Html), Attributes(..))
import           ComponentHelpers
import           Events
import           Rendering
import           Configuration
