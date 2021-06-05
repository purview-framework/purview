{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Expirement4 where

import Prelude hiding (div)
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

data Component value state = Component
  { render :: state -> (state, value)
  , state  :: state
  }

-- instance (Semigroup state, Semigroup value) => Semigroup (Component state value) where
--   Component renderA stateA <> Component renderB stateB =
--     Component (renderA <> renderB) (stateA <> stateB)

-- text = Component (, "text") Nothing
-- div = Component (, "div") "1"

class Render m where
  renderIt :: m a -> a

data Renderable = forall a. Render a => MkRenderable a

instance Render (Component a) where
  renderIt (Component render' state') =
    let
      (s1, v1) = render' state'
    in
      s1

--pack :: Render a => a -> Renderable
--pack = MkRenderable
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
