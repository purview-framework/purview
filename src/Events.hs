{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Events where

import Prelude hiding (print)
import           Data.Text (Text)
import           Data.Typeable
import           Data.Aeson
import           GHC.Generics

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
  Event ::
    { event :: Text
    , message :: Value
    , location :: Maybe [Int]
    } -> Event

  StateChangeEvent
    :: ( Eq state
       , Typeable state
       , ToJSON state
       , FromJSON state)
    => (state -> state) -> Maybe [Int] -> Event

instance Show Event where
  show (Event event message location) =
    show $ "{ event: "
      <> show event
      <> ", message: "
      <> show message
      <> ", location: "
      <> show location <> " }"
  show (StateChangeEvent _ location) =
    "{ event: \"newState\", location: " <> show location <> " }"

instance Eq Event where
  (Event { message=messageA, event=eventA, location=locationA })
    == (Event { message=messageB, event=eventB, location=locationB }) =
    eventA == eventB && messageA == messageB && locationA == locationB
  (Event {}) == _ = False
  (StateChangeEvent _ _) == _ = False

instance FromJSON Event where
  parseJSON (Object o) =
      Event <$> o .: "event" <*> (o .: "message") <*> o .: "location"
  parseJSON _ = error "fail"

{-|

This is for creating events that should go to a parent handler,
or sent back in to the same handler.

-}
data DirectedEvent a b where
  Parent :: ToJSON a => a -> DirectedEvent a b
  Self :: ToJSON b => b -> DirectedEvent a b
