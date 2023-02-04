{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Events where

import Prelude hiding (print)
import           Data.Text (Text)
import           Data.Typeable
import           Data.Aeson
import           GHC.Generics

type Identifier = Maybe [Int]
type ParentIdentifier = Identifier

{-|

This for events intended for the front end

-}
data ForFrontEndEvent m = ForFrontEndEvent
  { event :: Text
  , message :: m
  } deriving (Generic, Show)

instance ToJSON m => ToJSON (ForFrontEndEvent m) where
  toEncoding = genericToEncoding defaultOptions

{-|

These encapsulate events that come from the front end in addition to events
that are internal.  For example, state changes or messages being sent to
handlers higher up in the tree.

-}
data Event where
  FromFrontendEvent
    :: { kind :: Text
       -- ^ for example, "click" or "blur"
       , childLocation :: Identifier
       , location :: Identifier
       , value :: Maybe String
       }
    -> Event

  InternalEvent
    :: ( Show event
       , Eq event
       , Typeable event
       )
    => { event :: event
       , childId :: Identifier
       , handlerId :: Identifier
       }
    -> Event

  StateChangeEvent
    :: ( Eq state, Show state, Typeable state )
    => (state -> state) -> Identifier -> Event

instance Show Event where
  show (FromFrontendEvent event message location value) =
    show $ "{ event: "
      <> show event
      <> ", childLocation: "
      <> show message
      <> ", location: "
      <> show location
      <> ", value: "
      <> show value <> " }"

  show (StateChangeEvent _ location) =
    "{ event: \"newState\", location: " <> show location <> " }"

  show (InternalEvent event childId handlerId)
    =  "{ event: " <> show event
    <> ", childId: " <> show childId
    <> ", handlerId: " <> show handlerId
    <> " }"

instance Eq Event where
  (FromFrontendEvent { childLocation=messageA, kind=eventA, location=locationA, value=valueA })
    == (FromFrontendEvent { childLocation=messageB, kind=eventB, location=locationB, value=valueB }) =
    eventA == eventB && messageA == messageB && locationA == locationB && valueA == valueB
  (FromFrontendEvent {}) == _ = False

  (StateChangeEvent _ _) == _ = False

  (InternalEvent event childId handlerId) == (InternalEvent event' childId' handlerId') =
    case cast event of
      Just castEvent -> childId == childId' && handlerId == handlerId' && castEvent == event'
      Nothing        -> False
  (InternalEvent {}) == _ = False


instance FromJSON Event where
  parseJSON (Object o) =
      FromFrontendEvent <$> o .: "event" <*> (o .: "childLocation") <*> o .: "location" <*> o .:? "value"
  parseJSON _ = error "fail"

{-|

This is for creating events that should go to a parent handler,
or sent back in to the same handler.

-}
data DirectedEvent a b where
  Parent :: (Show a, Eq a) => a -> DirectedEvent a b
  Self :: (Show b, Eq b) => b -> DirectedEvent a b

