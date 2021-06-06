{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Expirement4 where

import Prelude hiding (div)

import Control.Comonad

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
data UI action

-- type Componen :: (* -> *) -> (* -> *) -> *
-- w is the comonad
-- m is the monad
type Componen w m = w (UI (m ()))

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

data Component value = forall state. Component
  { render :: state -> (state, value)
  , state  :: state
  }

-- instance (Semigroup state, Semigroup value) => Semigroup (Component state value) where
--   Component renderA stateA <> Component renderB stateB =
--     Component (renderA <> renderB) (stateA <> stateB)

text = Component (, "text") Nothing
div = Component (, "div") "1"

x = [text, div]

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
