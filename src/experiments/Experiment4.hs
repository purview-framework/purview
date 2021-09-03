{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Expirement4 where

import Prelude hiding (div)

import Control.Monad.State
import Control.Comonad
import Control.Comonad.Store

-- import Control.Monad.State.Strict

-- can I build a shadow state?

data World a = World a (World a) | Nil

-- x = World (pack text) (World (pack div) Nil)

-- r :: Render a b => World a -> a
-- r (World c _) =
--   let render' = renderIt c
--   in undefined

-- type World state value = (state, value)

-- https://www.twitch.tv/videos/938292097?filter=all&sort=time

-- https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html

-- type role UI phantom
-- type UI :: * -> *
-- data UI action

-- type Componen :: (* -> *) -> (* -> *) -> *
-- w is the comonad
-- m is the monad
-- type Componen w m = w (UI (m ()))

-- A pairing between functors f and g.

-- This asserts that any sums in f can annihilate any products in g, and vice versa.

-- This library provides some useful pairings, and ways of lifting pairings over various
-- constructions on Functors.
type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

-- explore :: forall w m
--          . Comonad w
--         => Pairing m w
--         -> w (UI (m ()))
--         -> IO ()
-- explore = undefined

-- For example, you can't have a list of polymorphic functions,
-- say [forall a. [a] -> [a]].  But you can wrap it like this:
-- newtype ListList = LL { unLL :: forall a. [a] -> [a] }
-- Now [ListList] is a perfectly fine type.  The downside is that you have
-- to wrap and unwrap, with LL and unLL, which is tiresome.

-- Well, user actions should result in movement to a new application state,
-- so we should choose our actions from some monad which pairs with our comonad.

-- can I represent the user action and put it into a monad?
-- data Store s a = Store
--   { here :: s
--   , view :: s -> a
--   }
--
-- instance Functor (Store s) where
--   fmap f (Store here' view') =
--     -- just extend it via composition
--     Store here' (f . view')
--
-- instance Comonad (Store s) where
--   extract (Store here' view') = view' here'
--   duplicate (Store here' view') =
--     -- hmm
--     Store here' (\next -> Store next view')

-- hmm so we can get user interactions involved here?
-- move :: s -> Store s a -> Store s a
-- move s store = view (duplicate store) s

-- Kmett (2011) defines a monad Co w which is constructed
-- from a comonad w. For our purposes, we think of its
-- actions as selecting some possible future state from a
-- collection of future states described by w. Figure 2 defines
-- the Co w monad, and the select function which selects a
-- future state

newtype Co w a = Co
  { runCo :: forall r. w (a -> r) -> r }
  deriving (Functor)

-- from http://comonad.com/reader/2011/monads-from-comonads/
instance Comonad w => Applicative (Co w) where
  pure a = Co (`extract` a)
  mf <*> ma = mf >>= \f -> fmap f ma

instance Comonad w => Monad (Co w) where
  Co k >>= f = Co (k . extend (\wa a -> runCo (f a) wa))

select :: Comonad w => Co w (a -> b) -> w a -> w b
select co w = runCo co (extend dist w)
  where dist fs f = fmap f fs

-- so... how does this work then?
-- moveT :: s -> Co (Store s) ()
-- moveT s = Co (\w -> view w s ())

ticker = store render 0 where
  render count send =
    [ div ]

data Attr = forall m. Evt (String -> m ())

data Flub
  = Flub String [ Attr ] [ Flub ]
  | Text String

button = Flub "button"
str = Text
onClick = Evt

newtype Identity a = Identity { runIdentity :: a }

-- data UI action
-- newtype Co w a = Co
--   { runCo :: forall r. w (a -> r) -> r }
--   deriving (Functor)

type Handler a = a -> [()]

type UI a = Handler a -> [Flub]

fre :: UI (Co (StoreT Int Identity) ())
fre send = do
  send (modify ((+) (1 :: Int)))
  [ button [] [] ]

-- render :: Int -> UI (Co (StoreT Int Identity) ())
-- flonk :: Int -> UI (Co (StoreT Int Identity) ())
-- flonk count send =
--    [ button [ onClick (\_ -> (send (modify ((+) 1)))) ] [ str (show count) ]]
--
-- goal = store render 0 where
--   render :: Int -> (m () -> IO ()) -> [Flub]
--   render count send =
--     [ button [ onClick $ const (send (modify ((+) 1))) ] [ str (show count) ]]

-- A specification for a user interface will be described by
-- a comonad w which describes the type of reachable next states.
-- The Co w monad for that comonad will describe the transitions
-- which are allowed.

--
-- I still don't know what to do with onClick
--

data Component value = forall state. Component
  { render :: state -> (state, value)
  , state  :: state
  }

text = Component (, "text") Nothing
div = Component (, "div") "1"

wumbo = Component (, [text, div]) Nothing
wumbus = Component (, [wumbo, wumbo]) Nothing

-- vague = okay wumbus

x = [text, div]

-- okay Component{render=render, state=state} =
--   let (oldState, value) = render state
--   in value
--
-- hmm = fmap okay x

-- soo, now we have:
-- a way to persist the state
-- a way for the state to change _in_ the component
-- but no way for user events to have an effect?

-- y = let z = (render text)
    -- in undefined

-- y =
--   let (Component r s) = text
--   in undefined

-- newtype ListList = LL { unLL :: Component }
--
-- saa = [LL text, LL div]

-- class Render state value where
--   renderIt :: state -> (state, value)

-- data Renderable = forall a b. Render a b => MkRenderable a b

-- instance Render (Component a b) where
--   renderIt (Component render' state') =
--     let
--       (s1, v1) = render' state'
--     in
--       state'
--
-- instance Render Renderable where
--   renderIt (MkRenderable c) = renderIt c
--
-- pack :: Render a => a -> Renderable
-- pack = MkRenderable

-- x = [pack text, pack div]
-- y = renderIt $ pack div

--
--bomba = Component
--  (\state -> (state, [pack text, pack div]))
--  Nothing

-- alright, let's do it one more fuckin time?

-- combo = Component
--   (\state -> text <> div)
--   Nothing

-- renderB state = (state, "renderB")
-- renderA state = (state, text "renderA" <> renderB)

--render' :: State Int String
--render' = (<> "h") . show <$> get
--
--newtype Attr a =
--  OnClick { unClick :: a }
--
--data Html m a = Html String [Attr a] [String]
--
--newtype State s a = State { runState :: s -> (s, a) }
--
--get :: State s s
--get = undefined
--
--put :: s -> State s ()
--put = undefined
--
--modify :: (s -> s) -> State s ()
--modify = undefined
--
--render'' :: State Int (Html m Int)
--render'' = do
--  -- modify (+ 1)
--  Html "div" [ OnClick get ] [ show "" ]
  -- show <$> get

-- so onclick could be an event... or a call to a function
-- that's called by the onclick
-- set state

--
-- input = do
--   result <- [ input ]
--   if
--

-- f = execState render' 0
-- f' = evalState  render' 0

--x = Component
--  { render = render' }

-- [ State ] [ Returned Value ] [ String ]

-- (State, Returned Value, String)
