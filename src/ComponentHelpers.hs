module ComponentHelpers where

import           Data.Typeable
import           Data.Bifunctor

import           Events
import           Component (Purview (..), Attributes (..))

{-|

This is most straightforward effect handler.  It can't send messages to itself
or to its parent.

For example, let's say you want to make a button that switches between saying
"up" or "down":

> view direction = onClick "toggle" $ button [ text direction ]
>
> handler = handler "up" reduce
>   where reduce "toggle" state = if state == "up" then "down" else "up"
>
> component = handler view

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

The same as handler, except you have access to the state at
application time.  Although rarely needed, if you have a long
running pure calculation you may need to use this to avoid
overwriting the state.

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

This handler gives you access to whichever monad you're running Purview with.

If you wanted to print something on the server every time someone clicked
a button:

> view direction = onClick "sayHello" $ button [ text "Say hello on the server" ]
>
> handler = effectHandler Nothing reduce
>   where reduce "sayHello" state = do
>           print "someone on the browser says hello!"
>           pure (const Nothing, [])
>
> component = handler view

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

The same as the effectHandler, except you have access to the state at
application time.  This allows you to prevent overwriting with old
state.

You'll want to use this if you're making a bunch of API requests to
build up a list of results.

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

receiver
  :: ( Show event
     , Eq event
     , Typeable event
     )
  => String -> (Maybe String -> event) -> Purview event m -> Purview event m
receiver name eventParser = Receiver Nothing Nothing name eventParser

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

For adding inline styles.  Good for dynamic parts of styling.

> blue = style "color: \"blue\";"
> blueButton = blue $ button [ text "I'm blue" ]

-}
istyle :: String -> Purview event m -> Purview event m
istyle str = Attribute $ Style ("-1", str)

{-|

This will send the event to the handler above it whenever "click" is triggered
on the frontend.  It will be bound to whichever 'HTML' is beneath it.

-}
onClick :: (Typeable event, Eq event, Show event) => event -> Purview event m -> Purview event m
onClick = Attribute . On "click" Nothing . const

{-|

This will send the event to the handler above it whenever "submit" is triggered
on the frontend.

-}
onSubmit :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onSubmit = Attribute . On "submit" Nothing

onBlur :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onBlur = Attribute . On "focusout" Nothing

onChange :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onChange = Attribute . On "change" Nothing

id' :: String -> Purview event m -> Purview event m
id' = Attribute . Generic "id"

class' :: String -> Purview event m -> Purview event m
class' = Attribute . Generic "class"

classes :: [String] -> Purview event m -> Purview event m
classes xs = Attribute . Generic "class" $ unwords xs

href :: String -> Purview event m -> Purview event m
href = Attribute . Generic "href"
