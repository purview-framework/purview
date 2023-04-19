{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- |

module Experiment25 where

import Data.Typeable
import Data.Coerce
import Unsafe.Coerce

data AST where
  Handler
    :: ( Typeable state )
    => { stateFn :: state -> state
       , state :: state
       , continuation :: AST
       }
    -> AST

  OtherHandler
    :: ( Typeable state )
    => (state -> state)
    -> AST

  Close :: AST

-- test = Handler
--
-- test2 = OtherHandler

tree = Handler id "" (Handler id "" Close)

applyEvent :: Typeable event => AST -> event -> AST
applyEvent ast event = case ast of
  Handler stateFn state cont -> case cast event of
    Just event' -> Handler stateFn (stateFn event') cont
    Nothing     -> Close
  _ -> Close

applyEvent' :: AST -> event -> AST
applyEvent' ast event = case ast of
  Handler stateFn state cont -> Handler stateFn (stateFn (unsafeCoerce event)) cont
  _ -> Close

data State where
  State :: Show state => { stateFun :: state -> state } -> State

test3 :: Integer -> State -> ()
test3 a b = undefined


test4 a b = (State b)

test5 y = test4 1 y

data Test where
  Test :: { x :: Show x => x } -> Test

data Secret where
  Secret :: Secret

y = Test Secret

-- instance Show Test where
--   show (Test f) = show f

data Temp = Temp | Not

-- test4 = test3 1 (State (\x -> Temp))
