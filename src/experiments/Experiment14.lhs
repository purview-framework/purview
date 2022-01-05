> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Experiment14 where

> import Data.Aeson
> import Data.Aeson.Types
> import Data.Typeable
> import GHC.Generics
> import Lib

So the goal is to get from the Value to an actual Action

> event' = "{\"event\":\"click\",\"message\":\"click\"}"

As is that parses into 'String "click"'

> decoded = decode event' :: Maybe FromEvent

Just (FromEvent {event = "click", message = String "click"})

> string' = case decoded of
>   Just (FromEvent _ msg) -> fromJSON msg :: Result String
>   Nothing -> error "no"

So that gets us there, but we're explicity stating what it should be.

Hmm.

> data Handler a where
>   Handle
>       :: (Typeable action, FromJSON action)
>       => state
>       -> (action -> state)
>       -> Handler a

> data Adjust = Increment | Decrement
>   deriving Generic
>
> instance FromJSON Adjust where
>
> handler = Handle 0 handle
>   where handle Increment = 1
>         handle Decrement = 0

Alright now we have something more realistic

> x :: Handler a -> Value -> Handler a
> x comp action = case comp of
>   Handle st handle -> case (result $ fromJSON action) of
>     Just action' -> Handle (handle action') handle

And now we're back to the problem

> result (Success a) = Just a

Or is that the whole solution?  Use fromJSON instead of cast?
