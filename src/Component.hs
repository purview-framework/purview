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
data Attributes action where
  On :: ToJSON action => String -> action -> Attributes action
  -- ^ part of creating handlers for different events, e.g. On "click"
  Style :: String -> Attributes action
  -- ^ inline css
  Generic :: String -> String -> Attributes action
  -- ^ for creating new Attributes to put on HTML, e.g. Generic "type" "radio" for type="radio".

instance Eq (Attributes action) where
  (Style a) == (Style b) = a == b
  (Style _) == _ = False

  (On kind action) == (On kind' action') = kind == kind' && encode action == encode action'
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
data Purview parentAction action m where
  Attribute :: Attributes action -> Purview parentAction action m -> Purview parentAction action m
  Text :: String -> Purview parentAction action m
  Html :: String -> [Purview parentAction action m] -> Purview parentAction action m
  Value :: Show a => a -> Purview parentAction action m

  -- | All the handlers boil down to this one.
  EffectHandler
    :: ( FromJSON newAction
       , ToJSON newAction
       , ToJSON parentAction
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
    -> (newAction-> state -> m (state -> state, [DirectedEvent parentAction newAction]))
    -- ^ Receive an action, change the state, and send messages
    -> (state -> Purview newAction any m)
    -- ^ Continuation
    -> Purview parentAction newAction m

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview parentAction action m
    -> Purview parentAction action m

  Hide :: Purview parentAction newAction m -> Purview parentAction any m

instance Show (Purview parentAction action m) where
  show (EffectHandler parentLocation location state _action cont) =
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

instance Eq (Purview parentAction action m) where
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
  :: ( FromJSON action
     , FromJSON state
     , ToJSON action
     , ToJSON parentAction
     , ToJSON state
     , Typeable state
     , Eq state
     , Applicative m
     )
  => state
  -- ^ The initial state
  -> (action -> state -> state)
  -- ^ The reducer, or how the state should change for an action
  -> (state -> Purview action any1 m)
  -- ^ The continuation / component to connect to
  -> Purview parentAction any2 m
simpleHandler state handler =
  effectHandler state (\action state -> pure (const $ handler action state, []))

{-|

More powerful than the 'simpleHandler', it can send messages to itself or its
parent.  You will also note that instead of just returning the new state, it
returns a function to transform the state.  This is because handlers run in
their own threads.

-}
messageHandler
  :: ( FromJSON action
     , FromJSON state
     , ToJSON action
     , ToJSON parentAction
     , ToJSON state
     , Typeable state
     , Eq state
     , Applicative m
     )
  => state
  -- ^ initial state
  -> (action -> state -> (state -> state, [DirectedEvent parentAction action]))
  -- ^ reducer
  -> (state -> Purview action any1 m)
  -- ^ continuation
  -> Purview parentAction any2 m
messageHandler state handler =
  effectHandler state (\action state -> pure (handler action state))

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
  :: ( FromJSON action
     , FromJSON state
     , ToJSON action
     , ToJSON parentAction
     , ToJSON state
     , Typeable state
     , Eq state
     )
  => state
  -- ^ initial state
  -> (action -> state -> m (state -> state, [DirectedEvent parentAction action]))
  -- ^ reducer (note the m!)
  -> (state -> Purview action any1 m)
  -- ^ continuation
  -> Purview parentAction any2 m
effectHandler state handler =
  Hide . EffectHandler Nothing Nothing state handler

{-|

This is for kicking off loading events.  Put it beneath one of your handlers
to send an event up to it, and it will only be sent once.

-}
once
  :: ToJSON action
  => ((action -> FromEvent) -> FromEvent)
  -> Purview parentAction action m
  -> Purview parentAction action m
once sendAction = Once sendAction False

{-

Helpers

-}

div :: [Purview parentAction action m] -> Purview parentAction action m
div = Html "div"

span :: [Purview parentAction action m] -> Purview parentAction action m
span = Html "span"

h1 :: [Purview parentAction action m] -> Purview parentAction action m
h1 = Html "h1"

h2 :: [Purview parentAction action m] -> Purview parentAction action m
h2 = Html "h2"

h3 :: [Purview parentAction action m] -> Purview parentAction action m
h3 = Html "h3"

h4 :: [Purview parentAction action m] -> Purview parentAction action m
h4 = Html "h4"

p :: [Purview parentAction action m] -> Purview parentAction action m
p = Html "p"

button :: [Purview parentAction action m] -> Purview parentAction action m
button = Html "button"

form :: [Purview parentAction action m] -> Purview parentAction action m
form = Html "form"

input :: [Purview parentAction action m] -> Purview parentAction action m
input = Html "input"

text :: String -> Purview parentAction action m
text = Text

{-|

For adding styles

> blue = style "color: \"blue\";"
> blueButton = blue $ button [ text "I'm blue" ]

-}
style :: String -> Purview parentAction action m -> Purview parentAction action m
style = Attribute . Style

{-|

This will send the action to the handler above it whenever "click" is triggered
on the frontend.  It will be bound to whichever 'HTML' is beneath it.

-}
onClick :: ToJSON action => action -> Purview parentAction action m -> Purview parentAction action m
onClick = Attribute . On "click"

{-|

This will send the action to the handler above it whenever "submit" is triggered
on the frontend.

-}
onSubmit :: ToJSON action => action -> Purview parentAction action m -> Purview parentAction action m
onSubmit = Attribute . On "submit"

identifier :: String -> Purview parentAction action m -> Purview parentAction action m
identifier = Attribute . Generic "id"

classes :: [String] -> Purview parentAction action m -> Purview parentAction action m
classes xs = Attribute . Generic "class" $ unwords xs
