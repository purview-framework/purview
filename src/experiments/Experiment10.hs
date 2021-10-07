{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Experiment10 where

data Attribute a
  = OnClick a
  | Style String

type Tag = String

data BaseComponent state actions = BaseComponent
  { state :: state
  , actions :: actions
  }

data Html actions
  = Html Tag [Attribute actions] [Html actions]
  | Base Actionable

-- x = BaseComponent "a" "b"

class Action a where
  runAction :: a -> b

data Actionable = forall a . Action a => MkActionable a

pack :: Action a => a -> Actionable
pack = MkActionable

test = BaseComponent "a" "b"

instance Action (BaseComponent a b) where

test2 = Html "" [] [Base (pack test)]
