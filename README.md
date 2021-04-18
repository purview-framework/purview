# bridge

An experiment to see how haskell can work with the new HOT TREND of sending updated HTML over websockets instead of silly old things like "APIs".  What front end?

The main library, that doesn't really do anything fancy yet, is in `src/Lib.hs`.  It's just replacing the entirety of the html each time it recieves an event from the front end.

Tested with artillery with 1000 users doing two requests (the increment & decrement below) and it didn't freak out, but 5000 certainly killed it.  It got about 200 req/s, which while not terrible for doing very little besides slapping some things together, I'd like to see how high it can get.  That only got the cheapest digital ocean vps to 7% CPU and used "fuck-all" ram so I'm not sure what's killing it.  I might have to do actual programming.  The response time is about 30 ms which is acceptable for an FPS counter.

Here's what a component looks like (see `app/Main.hs`):

```haskell
module Main where

import Prelude hiding (div)
import Lib

newtype State = State
  { count :: Int } deriving Show

defaultCounterState = State { count = 0 }

handlers' :: State -> String -> State
handlers' state message = case message of
  "increment" -> state { count = count state + 1 }
  "decrement" -> state { count = count state - 1 }

render' :: State -> Html
render' state =
  div [] [ div [onClick "increment"] [text "increment"]
         , text ("count: " <> show (count state))
         , div [onClick "decrement"] [text "decrement"]
         ]

counter = Component
  { initialize = defaultCounterState
  , handlers = handlers'
  , render = render'
  }

main = run counter
```

It's not awful, but stringly typed messages are gross and a sin against SPJ.  

Todo
* Actually diff the html (though who knows, sending the entire html every time might be smaller than your API request currently)
* Performance
* Type the messages or die trying
* Collect ideas
* Hey how do I do side effects?

Since it's still tiny it could go anywhere.  CSS styling the components?  Allow adding JS to optimistically update things that might fail?  The sky's the limit.
