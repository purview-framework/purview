{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Experiment5 where

import Data.IORef
import Control.Monad
import Control.Comonad

-- https://arthurxavierx.github.io/ComonadsForUIs.pdf


-- this is like (see kmett, 'dual' functors):
-- type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c
-- but at the typeclass level

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

-- define an infinite data type
data Stream a = Cons a (Stream a)
  deriving Functor

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate stream@(Cons a as) = Cons stream (duplicate as)

unfoldStream :: s -> (s -> (a, s)) -> Stream a
unfoldStream state next = Cons a (unfoldStream nextState next)
  where
    (a, nextState) = next state

-- the monad that goes with Stream is Sequence
data Sequence a = End a | Next (Sequence a)
  deriving Functor

instance Applicative Sequence where
  pure a = End a
  End f <*> End s = End (f s)

instance Monad Sequence where
  return = End

  (End a) >>= f = f a
  (Next next) >>= f = Next (next >>= f)

-- now the pairing between the two can be defined as:
instance Pairing Sequence Stream where
  pair f (End a) (Cons b _) = f a b
  pair f (Next next) (Cons _ stream) = pair f next stream

-- back to UI

-- with a console
data Console = Console { text :: String, action :: String -> IO () }

-- implementing an incrementing counter
-- the space of user interfaces (or states) is given by the Stream comonad
-- the movements / actions are given by the Sequence monad

counterComponent :: Component IO Stream Sequence Console
counterComponent = unfoldStream 0 (\state -> (render state, state + 1))
  where
    render :: Int -> UI IO Sequence Console
    render state = \send ->
      Console
        (show state)
        (\input -> send (Next (End ())))

move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)

explore :: (Comonad w, Pairing m w) => Component IO w m Console -> IO ()
explore component = do
  ref <- newIORef component
  forever $ do
    -- space :: Component IO w m Console
    space <- readIORef ref

    -- send receives an action dispatched by the UI
    -- and updates the state by moving around in space

    -- send :: m b -> IO ()
    let send action = writeIORef ref (move space action)

    let Console{ text=text, action=action } =
          extract space send -- extract the current interface

    -- the "view" step
    putStrLn text
    -- the action handling step
    input <- getLine
    action input

tapNCount = explore counterComponent

-- Onto the big boy
data Store s a = Store (s -> a) s
  deriving Functor

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- our old friend, the state monad
newtype State s a = State (s -> (a, s))
  deriving Functor

instance Applicative (State s) where
  pure a = State (a,)
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- expected type: s -> (b, s)
  -- fn :: s -> (a -> b, s)
  -- s ::s -> (a, s)
  State sa <*> State sb =
    State $ \s ->
        let (fn, s1) = sa s
            (a,  s2) = sb s1
        in (fn a, s2)

instance Monad (State s) where
  (State g) >>= f = State $ \s ->
    let (a, s') = g s
        State h = f a
    in h s'

-- fns for the state monad (it's good to redo these things
-- every now and again)
modify :: (s -> s) -> State s ()
modify transform = State (\state -> ((), transform state))

put :: s -> State s ()
put newState = State (const ((), newState))

-- need this to be able to move around in the space
instance Pairing (State s) (Store s) where
  pair f (State g) (Store get s) = f a (get s')
    where
      (a, s') = g s

listComponent :: Component IO (Store [String]) (State [String]) Console
listComponent = Store render []
  where
    render :: [String] -> UI IO (State [String]) Console
    render list = \send ->
      Console
        ("I've received: " <> show list)
        (\input ->
           send (if input == "" then put [] else modify (<> [input])))

-- and now we can just:
typeNReply = explore listComponent

-- mad component = do
--   -- get a name
--   forever $ do
--     setComponent "" text
--     -- getClick could be a channel?  tvar?
--     input <- getClick ""
--     action input
