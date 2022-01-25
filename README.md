# Purview

A work in progress implementation of Phoenix Liveview for Haskell

The main library, that doesn't really do anything fancy yet, is in `src/Lib.hs`.  It's just replacing the entirety of the html each time it receives an event from the front end.

### What it looks like

Here's what a component looks like (see `app/Main.hs`):

```haskell
module Main where

import Prelude hiding (div)
import Lib

upButton = onClick "up" $ div [ text "up" ]
downButton = onClick "down" $ div [ text "down" ]

handler = MessageHandler (0 :: Int) action
  where
    action "up"   state = state + 1
    action "down" state = state - 1

counter :: Int -> Purview a
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

logger = print

main = run logger (handler counter)
```

It's not awful, but stringly typed messages are gross and a sin against SPJ.  

### Todo
* Actually diff the html 
* Development environment (hot reloading, printing messages to/from the server)
* Allowing CSS + JS snippets in components
* Tests
* Performance
* Type the messages or die trying
* Collect ideas
* Hey how do I do side effects?

Since it's still tiny it could go anywhere.  CSS styling the components?  Allow adding JS to optimistically update things that might fail?  The sky's the limit.

### Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. `stack build`
3. `stack exec purview-exe` for just running the example above
4. `stack exec purview` for the ~ experimental ~ and not-currently working repl
