{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.Typeable

import           Events

{-|

Attributes are collected until an 'HTML' constructor is hit, where they
are applied during rendering.

-}
data Attributes event where
  On :: ( Eq event
        , Typeable event
        , Show event
        )
     => String
     -> Identifier
     -> (Maybe String -> event)  -- the string here is information from the browser
     -> Attributes event
        -- ^ part of creating handlers for different events, e.g. On "click"
  Style :: String -> Attributes event
        -- ^ inline css
  Generic :: String -> String -> Attributes event
        -- ^ for creating new Attributes to put on HTML, e.g. Generic "type" "radio" for type="radio".

instance Eq (Attributes event) where
  (Style a) == (Style b) = a == b
  (Style _) == _ = False

  (On kind ident _event) == (On kind' ident' _event') =
    kind == kind' && ident == ident'
  (On {}) == _ = False

  (Generic name value) == (Generic name' value') = name == name' && value == value'
  (Generic _ _) == _ = False

instance Show (Attributes event) where
  show (On kind ident evt) = "On " <> show kind <> " " <> show ident
  show (Style str) = "Style " <> show str
  show (Generic attrKey attrValue) = "Generic " <> show attrKey <> show attrValue

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

  EffectHandler
    :: ( Show state
       , Eq state
       , Typeable state
       , Typeable newEvent
       )
    => { parentIdentifier :: ParentIdentifier
       -- ^ The location of the parent effect handler (provided by prepareTree)
       , identifier       :: Identifier
       -- ^ The location of this effect handler (provided by prepareTree)
       , initialEvents    :: [DirectedEvent event newEvent]
       , state            :: state
       -- ^ The initial state
       , effectReducer    :: newEvent
                          -> state
                          -> m (state -> state, [DirectedEvent event newEvent])
       -- ^ Receive an event, change the state, and send messages
       , continuation     :: state -> Purview newEvent m
       }
    -> Purview event m

  Handler
    :: ( Show state
       , Eq state
       , Typeable state
       , Typeable newEvent
       )
    => { parentIdentifier :: ParentIdentifier
       , identifier       :: Identifier
       , initialEvents    :: [DirectedEvent event newEvent]
       , state            :: state
       , reducer          :: newEvent
                          -> state
                          -> (state -> state, [DirectedEvent event newEvent])
       , continuation     :: state -> Purview newEvent m
       }
    -> Purview event m

instance Show (Purview event m) where
  show (EffectHandler parentLocation location initialEvents state _event cont) =
    "EffectHandler "
      <> show parentLocation <> " "
      <> show location <> " "
      <> show state <> " "
      <> show (cont state)
  show (Handler parentLocation location initialEvents state _event cont) =
    "Handler "
      <> show parentLocation <> " "
      <> show location <> " "
      <> show state <> " "
      <> show (cont state)
  show (Attribute attrs cont) = "Attr " <> show attrs <> " " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value

instance Eq (Purview event m) where
  a == b = show a == show b

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

defaultHandler :: (() -> Purview () m) -> Purview () m
defaultHandler =
  Handler Nothing Nothing [] () (\event state -> (const (), []))

defaultEffectHandler :: Applicative m => (() -> Purview () m) -> Purview () m
defaultEffectHandler =
  EffectHandler Nothing Nothing [] () (\event state -> pure (const (), []))

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
onClick :: (Typeable event, Eq event, Show event) => event -> Purview event m -> Purview event m
onClick = Attribute . On "click" Nothing . const

{-|

This will send the event to the handler above it whenever "submit" is triggered
on the frontend.

-}
onSubmit :: (Typeable event, Eq event, Show event) => (Maybe String -> event) -> Purview event m -> Purview event m
onSubmit = Attribute . On "submit" Nothing

ident :: String -> Purview event m -> Purview event m
ident = Attribute . Generic "id"

classes :: [String] -> Purview event m -> Purview event m
classes xs = Attribute . Generic "class" $ unwords xs
