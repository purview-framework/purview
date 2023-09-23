# Purview

A simple, fun way to build websites with Haskell.  It's inspired by Phoenix LiveView, React, and Redux + Sagas.

The main points:
* It's server side rendered and uses websockets to communicate HTML updates and to receive events.
* State can be broken up into small components.
* Attributes flow down to concrete HTML, events bubble up to state handlers.
* Handlers can send further events to a parent handler or themselves

It's still in early development so expect things to break or be missing!

## What it looks like

```haskell
module Main where

import Prelude hiding (div)

import Purview 
import Purview.Server


data CountEvent = Increment | Decrement
  deriving (Show, Eq)

view :: Int -> Purview CountEvent m
view count = div
  [ h1 [ text (show count) ]
  , div [ onClick Increment $ button [ text "increment" ]
        , onClick Decrement $ button [ text "decrement" ]
        ]
  ]

-- arguments are initial actions, initial state, and then the reducer
countHandler = handler' [] (0 :: Int) reducer
  where
    reducer Increment state = (state + 1, [])  -- new events can be added in the []
    reducer Decrement state = (state - 1, [])

-- url is passed in to the top level component by `serve`
component url = countHandler view

main = serve defaultConfiguration component
```

More detailed docs on the use and particulars of Purview are mainly on [Hackage](https://hackage.haskell.org/package/purview).

### Overview of how it works

Using an example of getting the time, here's how events flow when the user clicks "check time"

1. The event is sent from the browser in a form like

   ```{ event: click, message: "checkTime", location: [0] }```
2. The event is put onto the channel for the event loop to process
3. By going down the tree it applies the event to the matched handler

   a. Any HTML changes are sent to the browser, completing the loop
5. The handler does its work in a green thread, creating a new event that looks like
   
   ```{ event: stateChange, fn: state -> state, location: [0] }```
7. The state change event is put onto the channel for the event loop to process
8. By going down the tree it applies the state change fn to the latest state in the tree, returning a new tree
9. Any HTML changes are sent to the browser, completing the loop

### Contributing

Anything is welcome, including examples or patterns you found nice.  There's still a lot to discover.

The roadmap is, loosely, determined by adding things required to build real websites.  The first two site-based goals:
1. The Purview website itself, which will have more in depth tutorials (so requiring at least navigation)
2. A stripe-based checkout (requiring communication with javascript)

### Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. `stack build`

### Running Tests

1. The same as above with stack and build
2. `stack test`
