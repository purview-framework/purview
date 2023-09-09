{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.Typeable
import           Events

type Hash = String

{-|

Attributes are collected until an 'HTML' constructor is hit, where they
are applied during rendering.

-}
data Attributes event where
  On :: ( Show event
        , Eq event
        , Typeable event
        )
     => String
     -> Identifier
     -> (Maybe String -> event)  -- the string here is information from the browser
     -> Attributes event
        -- ^ part of creating handlers for different events, e.g. On "click"
  Style :: (Hash, String) -> Attributes event
        -- ^ hash of the css, the css
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

  Receiver
    :: ( Show event
       , Eq event
       , Typeable event
       )
    => { parentIdentifier :: ParentIdentifier
       , identifier :: Identifier
       , name :: String  -- the name to be used to send an event
       , eventHandler :: Maybe String -> event  -- what to do with an event from the fn
       }
    -> Purview event m

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
  show (Receiver parentLocation location name _) =
    "Receiver "
      <> show parentLocation <> " "
      <> show location <> " "
      <> show name
  show (Attribute attrs cont) = "Attr " <> show attrs <> " " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value

instance Eq (Purview event m) where
  a == b = show a == show b
