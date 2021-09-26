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

data Component messages = Component
  { state :: messages
  , handlers :: messages -> messages
  }

instance Show m => Show (Component m) where
  show (Component st _) = show st

data ActionA = N | S
  deriving (Generic, Show)

instance FromJSON ActionA where

instance ToJSON ActionA where

compA :: Component ActionA
compA = Component N handler
  where
    handler N = S
    handler S = N

data ActionB = E | W

compB :: Component ActionB
compB = Component E handler
  where
    handler E = W
    handler W = E

type family Sing :: k -> Type

-- data SomeComponent :: Type where
--   MkSomeComponent :: Component m -> SomeComponent

data SomeComponent = forall a. (Handler a, Show a) => MkSomeComponent a

newtype Html = Html [SomeComponent]

comps = Html [MkSomeComponent compA]

class Handler m where
  handle :: m -> ByteString -> m

instance FromJSON m => Handler (Component m) where
  handle (Component state handler) message =
    let maybeMessage = decode message
    in case maybeMessage of
      (Just m) -> Component (handler m) handler
      Nothing -> Component state handler

instance Handler SomeComponent where
  handle (MkSomeComponent c) message = MkSomeComponent $ handle c message

-- type Handler :: * -> Constraint
-- class Handler m where
--   handle :: m -> ByteString -> m

-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

instance Show SomeComponent where
  show (MkSomeComponent c) = show c

runMessage :: ByteString -> Html -> Html
runMessage msg (Html (x:xs)) =
  let y = handle x msg
  in Html [y]

check =
  let Html (x:xs) = runMessage (pack "\"N\"") comps
  in show x
--  let y = parseJSON msg
--      MkSomeComponent (Component handler) = x
--  in Html [MkSomeComponent (Component (handler y))]
