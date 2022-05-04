{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Events where

import           Data.Text (Text)
import           Data.Typeable
import           Data.Aeson
import           GHC.Generics

data Event m = Event
  { event :: Text
  , message :: m
  } deriving (Generic, Show)

data FromEvent = FromEvent
  { event :: Text
  , message :: Value
  , location :: Maybe [Int]
  } deriving (Show, Eq, Generic)

instance FromJSON FromEvent where
  parseJSON (Object o) =
      FromEvent <$> o .: "event" <*> (o .: "message") <*> o .: "location"
  parseJSON _ = error "fail"

instance ToJSON m => ToJSON (Event m) where
  toEncoding = genericToEncoding defaultOptions

data StateChangeEvent where
  StateChangeEvent
    :: ( Eq state
       , Typeable state
       , ToJSON state
       , FromJSON state)
    => (state -> state) -> Maybe [Int] -> StateChangeEvent

instance Show StateChangeEvent where
  show (StateChangeEvent fn location) = "StateChangeEvent " <> show location

-- data StateChangeEvent = StateChangeEvent
--   { event :: (state -> state)
--   , location :: Maybe [Int]
--   }

{-|

This is for creating events that should go to a parent handler,
or sent back in to the same handler.

-}
data DirectedEvent a b = Parent a | Self b
  deriving (Generic, Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (DirectedEvent a b) where
  toEncoding = genericToEncoding defaultOptions
