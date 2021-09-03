{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Experiment8 where

import Data.IORef
import Control.Monad
import Control.Comonad

import Control.Monad.State
import Control.Comonad.Store


-- https://arthurxavierx.github.io/ComonadsForUIs.pdf


-- this is like (see kmett, 'dual' functors):
-- type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c
-- but as a typeclass
class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> c) -> f a -> g b -> c

-- abstract out IO of type UI action a = (action -> IO ()) -> a
-- type UI base action a = (action -> base ()) -> a

-- represents the space of all possible user interfaces, with
-- extract (from the comonad) giving us the current view

-- further abstracted
type UI base m a = (m () -> base ()) -> a

-- base: manages the internal state
-- m: defines the user actions
-- a: output

-- a component is defined as:
type Component base w m a = w (UI base m a)
-- equivalent to: type Component base w m a = w ((m () -> base ()) -> a)

-- base: the monad managing the internal state
-- w: a comonad representing the space of states
-- m: a monad representing the user actions
-- a: the html / w/e

--           explore :: (Comonad w, Pairing m w) => Component base w m a -> base ()
-- expanded: explore :: (Comonad w, Pairing m w) => w (m () -> base () -> a) -> base ()

-- with a console
-- data Console = Console { text :: String, action :: String -> IO () }

data Web action = Web { html :: String, action :: action -> IO () }

move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)

explore :: (Comonad w, Pairing m w) => Component IO w m (Web a) -> IO ()
explore component = do
  ref <- newIORef component

  forever $ do
    space <- readIORef ref

    let send action = writeIORef ref (move space action)

    let Web{ html=html, action=action } = extract space send

    -- so somehow I have to put this html into the actual webpage?
    -- I'm not sure how to do this?
    putStrLn html

    -- input <- readChannel

    -- what a terrible time this is
    -- action input

data Actions
  = Add
  | Remove

-- I wonder what I should try?
listComponent :: Component IO (Store [String]) (State [String]) (Web Actions)
listComponent = undefined
