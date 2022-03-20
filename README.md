# Purview

A work in progress implementation of Phoenix Liveview for Haskell

The main library is in `src/Purview.hs`. 

### What it looks like

Here's what a component looks like (see `app/Main.hs`):

```haskell
module Main where

import Prelude hiding (div)
import Data.Aeson
import Data.Aeson.TH

import Purview

data Direction = Up | Down

$(deriveJSON defaultOptions ''Direction)

upButton = onClick Up $ div [ text "up" ]
downButton = onClick Down $ div [ text "down" ]

handler = messageHandler (0 :: Int) action
  where
    action Up   state = state + 1
    action Down state = state - 1

counter :: Int -> Purview Direction
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

logger = print

main = run logger (handler counter)
```

### Todo
* Development environment (hot reloading, printing messages to/from the server)
* Allowing CSS + JS snippets in components
* Performance
* Collect ideas

Since it's still tiny it could go anywhere.  CSS styling the components?  Allow adding JS to optimistically update things that might fail?  The sky's the limit.

### Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. `stack build`
3. `stack exec purview-exe` for just running the example above
4. `stack exec purview` for the ~ experimental ~ and not-currently working repl

### Running Tests

1. The same as above with stack and build
2. `stack test`
