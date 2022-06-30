{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Experiment20 where

import Data.Proxy

data Html = Html

data Carrier a where
  Carrier
    ::
      { actions :: Proxy a
      , component :: Html
      }
    -> Carrier a

data Network where
  Div
    :: Carrier a
    -> String
    -> (Carrier a -> Network)

  Handler
    :: Carrier parentAction
    -> state
    -> (action -> state -> (state, [Either parentAction action]))
    -> (Carrier action -> Network)

  Nil :: Carrier a -> Network

test =
  let
    startingNode = Carrier
      { actions = Proxy :: Proxy ()
      , component = Html
      }
    handler incoming =
      Handler incoming "" (\action state -> (state, [Left ""]))
  in
    handler startingNode startingNode
--              ^ sadly the following error is here instead of on the handler
-- src/experiments/Experiment20.hs:42:13-24: error: …
--     • Couldn't match type ‘()’ with ‘[Char]’
--       Expected: Carrier String
--         Actual: Carrier ()

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- instance Functor Box where
--   fmap _ (Box { actions, component }) = Box Proxy component

-- mkHandler :: state -> (action -> state -> state) -> (state -> Box)
-- mkHandler = undefined

temp :: Proxy Int
temp = undefined

-- implicit parameters?

-- data Fancy actions where
--   Handler
--     :: (?parentAction :: parentActions)
--     => state
--     -> (action -> state -> state)
--     -> (state -> Fancy action)
--     -> Fancy actions
--
--   Nil :: Fancy actions
--
-- test2 = Handler "" (\action state -> state) (\state -> Nil)
--
-- test =
--   let
--     ?parentAction = ""
--   in
--     Handler "" (\action state -> state) (\state -> test2)

-- no
