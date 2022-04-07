{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Events where

import           Data.Text (Text)
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

data DirectedEvent a b = Parent a | Self b
  deriving (Generic, Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (DirectedEvent a b) where
  toEncoding = genericToEncoding defaultOptions
