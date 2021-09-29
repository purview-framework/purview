{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Experiment12 where

import           Data.Aeson
import           Data.Kind
import           Data.ByteString.Lazy hiding (pack)
import           Data.ByteString.Lazy.Char8
import           GHC.Generics

data Component state messages = Component
  { state :: state
  , handlers :: messages -> state
  }

instance Show s => Show (Component s m) where
  show (Component st _) = show st

data ActionA = N | S
  deriving (Generic, Show)

instance FromJSON ActionA where

instance ToJSON ActionA where

compA :: Component ActionA ActionA
compA = Component N handler
  where
    handler N = S
    handler S = N

data ActionB = E | W
  deriving (Generic, Show)

instance FromJSON ActionB where

compB :: Component ActionB ActionB
compB = Component E handler
  where
    handler E = W
    handler W = E

data SomeComponent = forall a. (Handler a, Show a) => MkSomeComponent a

newtype Html = Html [SomeComponent]

comps = Html [MkSomeComponent compA, MkSomeComponent compB]

class Handler m where
  handle :: m -> ByteString -> m

instance FromJSON m => Handler (Component s m) where
  handle (Component state handler) message =
    case decode message of
      Just m  -> Component (handler m) handler
      Nothing -> Component state handler

instance Handler SomeComponent where
  handle (MkSomeComponent c) message = MkSomeComponent $ handle c message

instance Show SomeComponent where
  show (MkSomeComponent c) = show c

runMessage :: ByteString -> Html -> Html
runMessage msg (Html (x:xs)) =
  let y = handle x msg
  in Html [y]

check =
  let Html (x:xs) = runMessage (pack "\"N\"") comps
  in show x
