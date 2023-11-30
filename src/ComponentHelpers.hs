module ComponentHelpers where

import           Data.Typeable
import           Data.Bifunctor

import           Events
import           Component (Purview (..), Attributes (..))

{-|

This is the pure handler, for when you don't need access to IO.  Events
are still handled within a green thread so it's possible to overwrite
state, just something to be aware of.

__Example__:

Let's say you want to make a button that switches between saying
"up" or "down":

@
view direction = onClick "toggle" $ button [ text direction ]

toggleHandler = handler [] "up" reducer
  where reducer "toggle" state =
          let newState = if state == "up" then "down" else "up"
          in (const newState, [])

component = toggleHandler view
@

Or typed out in longer form:

@
type State = String
type Event = String

reducer :: Event -> State -> (State -> State, [DirectedEvent parentEvent Event])
reducer event state = case event of
  "up"   -> (const "down", [])
  "down" -> (const "up", [])

toggleHandler :: (State -> Purview Event m) -> Purview parentEvent m
toggleHandler = handler [] "up" reducer

component :: Purview parentEvent m
component = toggleHandler view
@

Note that parentEvent is left unspecified as this handler doesn't send
any events to a parent, so it can be plugged in anywhere.  If you did
want to send events, the reducer looks like this:

@
reducer :: String -> String -> (String -> String, [DirectedEvent String String])
reducer event state = case event of
  "up"   -> (const "down", [Self "down"])
  "down" -> (const "up", [Parent "clickedDown"])
@

Which is about all there is to sending more events.
-}
handler
  :: ( Typeable event
     , Show state
     , Eq state
     , Typeable state
     )
  => [DirectedEvent parentEvent event]
  -- ^ Initial events to fire
  -> state
  -- ^ The initial state
  -> (event -> state -> (state -> state, [DirectedEvent parentEvent event]))
  -- ^ The reducer, or how the state should change for an event
  -> (state -> Purview event m)
  -- ^ The continuation / component to connect to
  -> Purview parentEvent m
handler initEvents state reducer cont =
  Handler Nothing Nothing initEvents state reducer cont

{-|

This provides a shorthand for when you know you want to overwrite
the state on each event.

__Example__:

@
view direction = onClick "toggle" $ button [ text direction ]

toggleHandler = handler' [] "up" reducer
  where reducer "toggle" state =
          let newState = if state == "up" then "down" else "up"
          -- note it's just newState, not const newState
          in (newState, [])

component = toggleHandler view
-}
handler'
  :: ( Typeable event
     , Show state
     , Eq state
     , Typeable state
     )
  => [DirectedEvent parentEvent event]
  -- ^ Initial events to fire
  -> state
  -- ^ The initial state
  -> (event -> state -> (state, [DirectedEvent parentEvent event]))
  -- ^ The reducer, or how the state should change for an event
  -> (state -> Purview event m)
  -- ^ The continuation / component to connect to
  -> Purview parentEvent m
handler' initEvents state reducer cont =
  Handler Nothing Nothing initEvents state (constReducer reducer) cont
  where constReducer reducer event state =
          let (newState, events) = reducer event state
          in (const newState, events)


{-|

This handler gives you access to whichever monad you're running Purview with.

__Example:__

If you wanted to print something on the server every time someone clicked
a button:

@
view _ = onClick "sayHello" $ button [ text "Say hello on the server" ]

handler = effectHandler [] () reduce
  where reduce "sayHello" state = do
          print "Someone on the browser says hello!"
          pure (const (), [])

component = handler view
@
-}
effectHandler
  :: ( Typeable event
     , Show state
     , Eq state
     , Typeable state
     )
  => [DirectedEvent parentEvent event]
  -- ^ Initial events to fire
  -> state
  -- ^ initial state
  -> (event -> state -> m (state -> state, [DirectedEvent parentEvent event]))
  -- ^ reducer (note the m!)
  -> (state -> Purview event m)
  -- ^ continuation
  -> Purview parentEvent m
effectHandler initEvents state reducer cont =
  EffectHandler Nothing Nothing initEvents state reducer cont

{-|
To mirror handler', a shorthand for when you know you want to overwrite state.
-}
effectHandler'
  :: ( Typeable event
     , Show state
     , Eq state
     , Typeable state
     , Functor m
     )
  => [DirectedEvent parentEvent event]
  -- ^ Initial events to fire
  -> state
  -- ^ initial state
  -> (event -> state -> m (state, [DirectedEvent parentEvent event]))
  -- ^ reducer (note the m!)
  -> (state -> Purview event m)
  -- ^ continuation
  -> Purview parentEvent m
effectHandler' initEvents state reducer cont =
  EffectHandler Nothing Nothing initEvents state (constReducer reducer) cont
  where constReducer reducer event state = fmap (Data.Bifunctor.first const) (reducer event state)

{-|
For receiving events from Javascript.  In addition to the name and an event
producer, the receiver takes in a state and child and passes it through for
(hopefully) more natural composition with handlers.

__Example:__

This receives an event from javascript every 1 second and increments
the count.

@
component count = div [ text (show count) ]

countHandler = handler' [] (0 :: Int) reducer
  where
    reducer "increment" state = (state + 1, [])
    reducer "decrement" state = (state - 1, [])

countReceiver = receiver "incrementReceiver" (const "increment")

render = countHandler . countReceiver $ component

jsCounter = [r|
  const startCount = () => {
    window.setInterval(() => {
      -- sendEvent is added to the window by Purview and all that's
      -- needed.  Purview finds the receiver by name.
      sendEvent("incrementReceiver", "increment")
    }, 1000)
  }
  startCount()
|]

main = serve defaultConfiguration { javascript=jsCounter } render
@
-}
receiver
  :: ( Show event
     , Eq event
     , Typeable event
     )
  => String -> (Maybe String -> event) -> (state -> Purview event m) -> state -> Purview event m
receiver name eventParser child state = Receiver Nothing Nothing name eventParser child state

{-

Helpers

-}

div :: [Purview event m] -> Purview event m
div = Html "div"

span :: [Purview event m] -> Purview event m
span = Html "span"

h1 :: [Purview event m] -> Purview event m
h1 = Html "h1"

h2 :: [Purview event m] -> Purview event m
h2 = Html "h2"

h3 :: [Purview event m] -> Purview event m
h3 = Html "h3"

h4 :: [Purview event m] -> Purview event m
h4 = Html "h4"

p :: [Purview event m] -> Purview event m
p = Html "p"

a :: [Purview event m] -> Purview event m
a = Html "a"

ul :: [Purview event m] -> Purview event m
ul = Html "ul"

li :: [Purview event m] -> Purview event m
li = Html "li"

button :: [Purview event m] -> Purview event m
button = Html "button"

form :: [Purview event m] -> Purview event m
form = Html "form"

input :: [Purview event m] -> Purview event m
input = Html "input"

text :: String -> Purview event m
text = Text

{-|
For adding an "id" to HTML
-}
id' :: String -> Purview event m -> Purview event m
id' = Attribute . Generic "id"

{-|
For adding a "class" to HTML
-}
class' :: String -> Purview event m -> Purview event m
class' = Attribute . Generic "class"

classes :: [String] -> Purview event m -> Purview event m
classes xs = Attribute . Generic "class" $ unwords xs

href :: String -> Purview event m -> Purview event m
href = Attribute . Generic "href"

{-|

For adding inline styles.  Good for dynamic parts of styling, as
the style QuasiQuoter does not support variables.

__Example:__

Varying a color based on input:

@
submitButton valid =
  let
    borderColor = if valid then "green" else "red"
    borderStyle = "border-color: " <> borderColor <> ";"
  in
    istyle borderStyle $ button [ text "Submit" ]
@
-}
istyle :: String -> Purview event m -> Purview event m
istyle = Attribute . Generic "style"

{-|

This will send the event to the handler above it whenever "click" is triggered
on the frontend.  It will be bound to whichever concrete HTML is beneath it.


__Example:__

To send a string based event:

@
toggleButton :: Purview String m
toggleButton = onClick "toggle" $ button []
@

To send a better typed event:

@
data Toggle = Toggle
  deriving (Show, Eq)

toggleButton :: Purview Toggle m
toggleButton = onClick Toggle $ button []
@

Note how the type changed to show which kind of event is produced.

-}
onClick :: (Typeable event, Eq event, Show event) => event -> Purview event m -> Purview event m
onClick = Attribute . On "click" Nothing . const

{-|

This will send the event to the handler above it whenever "submit" is triggered
on the frontend.  It takes a function to transform the value received into
an event for handlers, this can be a good spot to debug trace and see what
is being received from the browser.

The form produces JSON so the handling function can also be used to parse
the form, or you can throw it up as a string for the handler to parse.

__Example:__

@
nameAttr = Attribute . Generic "name"

data FormEvent = Submitted String
  deriving (Show, Eq)

handleSubmit (Just val) = Submitted val
handleSubmit Nothing    = Submitted ""

component :: Purview FormEvent m
component = onSubmit handleSubmit $
  form
    [ nameAttr "text" $ input []
    ]
@
-}
onSubmit :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onSubmit = Attribute . On "submit" Nothing

{-|
This is triggered on focusout instead of blur to work with mobile
sites as well.  Like onSubmit it takes a value.

__Example:__

@
data AddressEvent = LineOneUpdated String
  deriving (Show, Eq)

handleBlur (Just val) = LineOneUpdated val
handleBlur Nothing    = LineOneUpdated ""

addressLineOne = onBlur handleBlur $ input []
@
-}
onBlur :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onBlur = Attribute . On "focusout" Nothing

{-|
Triggered on change

__Example:__

@
data AddressEvent = LineOneUpdated String
  deriving (Show, Eq)

handleChange (Just val) = LineOneUpdated val
handleChange Nothing    = LineOneUpdated ""

addressLineOne = onChange handleChange $ input []
@
-}
onChange :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onChange = Attribute . On "change" Nothing
