{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Experiment12 where

import           Data.Aeson
import           Data.ByteString.Lazy hiding (pack)
import           Data.ByteString.Lazy.Char8
import           GHC.Generics
import Control.Monad.Reader
import Data.Typeable
import Data.Dynamic

newtype ComponentT e m a = ComponentT { unwrap :: Reader String a }
  deriving (Functor, Applicative, Monad)

data Component state messages = Component
  { state :: state
  , handlers :: messages -> state
  } deriving Typeable

instance Show s => Show (Component s m) where
  show (Component state handlers) = show state

temp :: Component String String
temp = Component
  { state = "hello"
  , handlers = \message -> "feh"
  }

data SomeComponent = forall a. (Typeable a) => MkSomeComponent a

f = MkSomeComponent temp

y = case cast f of
  Just (Component state handlers) -> Component ("heck" :: String) handlers

-- test :: (Typeable a, Typeable b) => SomeComponent -> Maybe (Component a b)
-- test (MkSomeComponent a) = fromDynamic a
--
-- test3 :: SomeComponent -> Maybe (Component String String)
-- test3 (MkSomeComponent a) = cast a

-- test2 = case test f of
--   Just _ -> "ok"
--   Nothing -> "failed"

-- render =
--   let h = handlers temp
--   in runReader (h "")

-- mkComponent state handlers =
--   let c = Component state handlers
--   in runReader c ""

-- instance Show s => Show (Component s m) where
--   show (Component st _) = show st
--
-- data ActionA = N | S
--   deriving (Generic, Show)
--
-- instance FromJSON ActionA where
--
-- instance ToJSON ActionA where
--
-- compA :: Component ActionA ActionA
-- compA = Component N handler
--   where
--     handler N = S
--     handler S = N
--
-- data ActionB = E | W
--   deriving (Generic, Show)
--
-- instance FromJSON ActionB where
--
-- compB :: Component ActionB ActionB
-- compB = Component E handler
--   where
--     handler E = W
--     handler W = E
--
-- data SomeComponent = forall a. (Handler a, Show a) => MkSomeComponent a
--
-- newtype Html = Html [SomeComponent]
--
-- comps = Html [MkSomeComponent compA, MkSomeComponent compB]
--
-- class Handler m where
--   handle :: m -> ByteString -> m
--
-- instance FromJSON m => Handler (Component s m) where
--   handle (Component state handler) message =
--     case decode message of
--       Just m  -> Component (handler m) handler
--       Nothing -> Component state handler
--
-- instance Handler SomeComponent where
--   handle (MkSomeComponent c) message = MkSomeComponent $ handle c message
--
-- instance Show SomeComponent where
--   show (MkSomeComponent c) = show c
--
-- runMessage :: ByteString -> Html -> Html
-- runMessage msg (Html (x:xs)) =
--   let y = handle x msg
--   in Html [y]
--
-- check =
--   let Html (x:xs) = runMessage (pack "\"N\"") comps
--   in show x
