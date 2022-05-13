# Purview

A framework to build interactive UIs with Haskell.  It's inspired by Phoenix LiveView, React, and Redux + Sagas.

The main points:
* It's server side rendered and uses websockets to communicate HTML updates and to receive events.
* State can be broken up into small components.
* The approach is to provide useful atoms, with the user building up a kind of AST.
* Attributes flow down to concrete HTML, events bubble up to state handlers.

It's still in early development so expect things to break or be missing!

## What it looks like

Here's what a component looks like (see `experiments/Counter.hs`):

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

handler = messageHandler (0 :: Int) reducer
  where
    reducer Up   state = (const $ state + 1, [])
    reducer Down state = (const $ state - 1, [])

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

view = handler counter

main = Purview.run defaultConfiguration { component=view }
```

## Overview

### Adding attributes to HTML elements

Attributes flow down to concrete HTML.

For example, if you wanted to add a `style="color: blue;"` to a `div`:

``` haskell
blue = style "color: blue;"

blueDiv = blue (div [])
```

Calling `render blueDiv` will produce `<div style="color: blue;"></div>"`

If you wanted to have a blue div that's 50% of the width,

``` haskell
blue = style "color: blue;"
halfWidth = style "width: 50%;"

view = blue (halfWidth (div [])
```

Now `render view` will produce `<div style="color: blue; width: 50%;></div>`

As purview is non-prescriptive in what attributes you can give a `div`, or any other HTML element, you can create your own.

If you need `name` attribute put on `div`s or other HTML, you can do:

``` haskell
nameAttr = Attribute . Generic "name"

namedDiv = nameAttr "wumbo" (div [])
```

And `render namedDiv` will produce `<div name="wumbo"></div>`.  Eventually there will be more attributes-by-default like `style`, but for now expect to build up what you need!

### Creating new HTML elements

Just like you can add new attributes, you can also add new html elements.  For example, if you need a button

``` haskell
button = Html "button"

view = button [ text "click" ]
```

Now `render view` will produce `<button>click</button>`.  Like all the built in ones, attributes will flow down and be added to the button.

### Events

At the core of Purview are three event handlers, in order of increasing power:
1. `simpleHandler`: Used for just returning a new state.  No messages or effects.
2. `messageHandler`: Used when you need to send messages to the component itself or to its parent.
3. `effectHandler`: Used when you need the above and access to IO / your monad stack / algebraic effects.

The first two are just some sugar around `effectHandler`.

Handlers take an initial state and a reducer.  The reducer receives actions from anywhere below them in the tree, and returns the new state with a list of actions to send either to itself or up the tree to the parent.  The handler passes down the state to its child.  This is the core idea to make it all interactive.

For example, if we wanted to build something that fetched the server time on each click:

``` haskell
reducer action state = case action of
  "getTime" -> do
      time <- getCurrentTime
      pure (const $ Just time, [])

handler = effectHandler Nothing reducer

view time = div 
  [ onClick "getTime" $ button [ text "check time" ]
  , p [ text (show time) ]
  ]
  
component = handler view
```

Some things to note:
* The state is passed down to children.
* Events bubble up to the nearest handler where they are captured.
* `onClick` can wrap anything -- like other attributes it flows down to concrete HTML.
* The reducer is run in its own thread when an event is received, so you don't have to worry about slow operations locking the page.

### Overview of how it works

Using the above example of getting the time, here's how events flow when the user clicks "check time"

1. The event is sent from the browser in a form like `{ event: click, message: "checkTime", location: [0] }`
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
3. `stack exec purview-exe` for just running the example above
4. `stack exec purview` for the ~ experimental ~ and not-currently working repl

### Running Tests

1. The same as above with stack and build
2. `stack test`
