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
import           Unsafe.Coerce

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
  Event
    :: { kind :: Text
       -- ^ for example, "click" or "blur"
       , childLocation :: Identifier
       , location :: Identifier
       }
    -> Event

  AnyEvent
    :: ( Show event
       , Eq event
       )
    => { event :: event
       , childId :: Identifier
       , handlerId :: Identifier
       }
    -> Event

  StateChangeEvent
    :: ( Eq state, Show state )
    => (state -> state) -> Identifier -> Event

instance Show Event where
  show (Event event message location) =
    show $ "{ event: "
      <> show event
      <> ", childLocation: "
      <> show message
      <> ", location: "
      <> show location <> " }"

  show (StateChangeEvent _ location) =
    "{ event: \"newState\", location: " <> show location <> " }"

  show (AnyEvent event childId handlerId)
    =  "{ event: " <> show event
    <> ", childId: " <> show childId
    <> ", handlerId: " <> show handlerId
    <> " }"

instance Eq Event where
  (Event { childLocation=messageA, kind=eventA, location=locationA })
    == (Event { childLocation=messageB, kind=eventB, location=locationB }) =
    eventA == eventB && messageA == messageB && locationA == locationB
  (Event {}) == _ = False

  (StateChangeEvent _ _) == _ = False

  (AnyEvent event childId handlerId) == (AnyEvent event' childId' handlerId') =
      childId == childId' && handlerId == handlerId' && event == unsafeCoerce event'
  (AnyEvent {}) == _ = False


instance FromJSON Event where
  parseJSON (Object o) =
      Event <$> o .: "event" <*> (o .: "childLocation") <*> o .: "location"
  parseJSON _ = error "fail"

{-|

This is for creating events that should go to a parent handler,
or sent back in to the same handler.

-}
data DirectedEvent a b where
  Parent :: (Show a, Eq a) => a -> DirectedEvent a b
  Self :: (Show b, Eq b) => b -> DirectedEvent a b

-- data AnyEvent where
--   AnyEvent
--     :: ( Show event
--        , Typeable event
--        , Eq event
--        )
--     => { event :: event
--        , childId :: Identifier
--        , handlerId :: Identifier
--        } -> AnyEvent

-- TODO: these instances are incomplete (see now having identifiers)
-- instance Show AnyEvent where
--   show (AnyEvent evt _ _) = show evt
--
-- instance Eq AnyEvent where
--   (AnyEvent evt _ _) == (AnyEvent evt' _ _) = case cast evt' of
--     Just evt'' -> evt == evt''
--     Nothing    -> False
