{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.Aeson
import           Data.Typeable

import           Events

{-|

Attributes are collected until an 'HTML' constructor is hit, where they
are applied during rendering.

-}
data Attributes event where
  On :: ToJSON event => String -> event -> Attributes event
  -- ^ part of creating handlers for different events, e.g. On "click"
  Style :: String -> Attributes event
  -- ^ inline css
  Generic :: String -> String -> Attributes event
  -- ^ for creating new Attributes to put on HTML, e.g. Generic "type" "radio" for type="radio".

instance Eq (Attributes event) where
  (Style a) == (Style b) = a == b
  (Style _) == _ = False

  (On kind event) == (On kind' event') = kind == kind' && encode event == encode event'
  (On _ _) == _ = False

  (Generic name value) == (Generic name' value') = name == name' && value == value'
  (Generic _ _) == _ = False

type Identifier = Maybe [Int]
type ParentIdentifier = Identifier

{-|

This is what you end up building using the various helpers.  It's hopefully rare
that you have to use these directly, but it may be useful to better understand
what's happening behind the scenes.

-}
data Purview event m where
  Attribute :: Attributes event -> Purview event m -> Purview event m
  Text :: String -> Purview event m
  Html :: String -> [Purview event m] -> Purview event m
  Value :: Show a => a -> Purview event m

  -- | All the handlers boil down to this one.
  EffectHandler
    :: ( FromJSON newEvent
       , ToJSON newEvent
       , ToJSON parentEvent
       , FromJSON state
       , ToJSON state
       , Typeable state
       , Eq state
       )
    => ParentIdentifier
    -- ^ The location of the parent effect handler (provided by prepareTree)
    -> Identifier
    -- ^ The location of this effect handler (provided by prepareTree)
    -> state
    -- ^ The initial state
    -> (newEvent-> state -> m (state -> state, [DirectedEvent parentEvent newEvent]))
    -- ^ Receive an event, change the state, and send messages
    -> (state -> Purview newEvent m)
    -- ^ Continuation
    -> Purview parentEvent m

  Once
    :: (ToJSON event)
    => ((event -> Event) -> Event)
    -> Bool  -- has run
    -> Purview event m
    -> Purview event m

  Hide :: Purview newEvent m -> Purview any m

instance Show (Purview event m) where
  show (EffectHandler parentLocation location state _event cont) =
    "EffectHandler "
    <> show parentLocation <> " "
    <> show location <> " "
    <> show (encode state) <> " "
    <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute _attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value
  show (Hide a) = "Hide " <> show a

instance Eq (Purview event m) where
  a == b = show a == show b

{-|

This is most straightforward effect handler.  It can't send messages to itself
or to its parent.

For example, let's say you want to make a button that switches between saying
"up" or "down":

> view direction = onClick "toggle" $ button [ text direction ]
>
> handler = simpleHandler "up" reduce
>   where reduce "toggle" state = if state == "up" then "down" else "up"
>
> component = handler view

-}
simpleHandler
  :: ( FromJSON event
     , FromJSON state
     , ToJSON event
     , ToJSON parentEvent
     , ToJSON state
     , Typeable state
     , Eq state
     , Applicative m
     )
  => state
  -- ^ The initial state
  -> (event -> state -> state)
  -- ^ The reducer, or how the state should change for an event
  -> (state -> Purview event m)
  -- ^ The continuation / component to connect to
  -> Purview parentEvent m
simpleHandler state handler =
  effectHandler state (\event state -> pure (const $ handler event state, []))

{-|

More powerful than the 'simpleHandler', it can send messages to itself or its
parent.  You will also note that instead of just returning the new state, it
returns a function to transform the state.  This is because handlers run in
their own threads.

-}
messageHandler
  :: ( FromJSON event
     , FromJSON state
     , ToJSON event
     , ToJSON parentEvent
     , ToJSON state
     , Typeable state
     , Eq state
     , Applicative m
     )
  => state
  -- ^ initial state
  -> (event -> state -> (state -> state, [DirectedEvent parentEvent event]))
  -- ^ reducer
  -> (state -> Purview event m)
  -- ^ continuation
  -> Purview parentEvent m
messageHandler state handler =
  effectHandler state (\event state -> pure (handler event state))

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
effectHandler
  :: ( FromJSON event
     , FromJSON state
     , ToJSON event
     , ToJSON parentEvent
     , ToJSON state
     , Typeable state
     , Eq state
     )
  => state
  -- ^ initial state
  -> (event -> state -> m (state -> state, [DirectedEvent parentEvent event]))
  -- ^ reducer (note the m!)
  -> (state -> Purview event m)
  -- ^ continuation
  -> Purview parentEvent m
effectHandler state handler =
  Hide . EffectHandler Nothing Nothing state handler

{-|

This is for kicking off loading events.  Put it beneath one of your handlers
to send an event up to it, and it will only be sent once.

-}
once
  :: ToJSON event
  => ((event -> Event) -> Event)
  -> Purview event m
  -> Purview event m
once sendEvent = Once sendEvent False

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

button :: [Purview event m] -> Purview event m
button = Html "button"

form :: [Purview event m] -> Purview event m
form = Html "form"

input :: [Purview event m] -> Purview event m
input = Html "input"

text :: String -> Purview event m
text = Text

{-|

For adding styles

> blue = style "color: \"blue\";"
> blueButton = blue $ button [ text "I'm blue" ]

-}
style :: String -> Purview event m -> Purview event m
style = Attribute . Style

{-|

This will send the event to the handler above it whenever "click" is triggered
on the frontend.  It will be bound to whichever 'HTML' is beneath it.

-}
onClick :: ToJSON event => event -> Purview event m -> Purview event m
onClick = Attribute . On "click"

{-|

This will send the event to the handler above it whenever "submit" is triggered
on the frontend.

-}
onSubmit :: ToJSON event => event -> Purview event m -> Purview event m
onSubmit = Attribute . On "submit"

identifier :: String -> Purview event m -> Purview event m
identifier = Attribute . Generic "id"

classes :: [String] -> Purview event m -> Purview event m
classes xs = Attribute . Generic "class" $ unwords xs
