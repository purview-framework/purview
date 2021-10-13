{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- |

module Experiment13 where

import Data.Typeable

{-

Maybe I should try a new technique that isn't based entirely
around a record

-}

-- data Purview a where
--   Text :: String -> Purview a
--   Html :: String -> Purview a -> Purview a
--   -- UseState :: state -> ((state, state -> ()) -> Purview a) -> Purview a
--   -- Connect ::
--   Handler
--     :: (Typeable messages)
--     => state
--     -> (messages -> state)
--     -> (state -> Purview messages)
--     -> Purview messages
--
-- t = Html "a"
--
-- x state = Html (show state) (Text (show state))
--
-- data Actions = Up | Down
--   deriving Show
--
-- y = Handler Up handle x
--   where
--     handle Up = Down
--     handle Down = Up
--
-- walk :: Actions -> Purview a -> String
-- walk action (Handler st handle rest) =
--   let newSt = maybe st handle (cast action)
--   in walk action (rest newSt)
-- walk _ (Html kind children) = kind

{-

Next thing to figure out is actions

State -> passes down the state and a way to set the state
OnChange -> runs when params passed in change, but once by default

-}

-- newtype Attribute a = OnClick a
--
-- data Purview a where
--   Text :: String -> Purview a
--   Html :: String -> [Attribute a] -> Purview a -> Purview a
--   State :: state -> ((state, state -> ()) -> Purview a) -> Purview a
--   -- Handler
--   -- OnChange
--   -- Once
--
-- data AnnPurview a where
--   AnnText :: Purview a -> AnnPurview a
--   AnnState :: String -> Purview a -> AnnPurview a
--
-- comp (state, setState) = Html "div" [OnClick (setState (state + 1))] (Text "hello")
--
-- state = State (0 :: Integer)
--
-- comb = state comp

{-

Somehow have to turn that setState into sending a message

Maybe it should just be done in terms of handler?

Imagine a handler with no state.  Maybe the message is just
analytics.

-}

newtype Attribute a = OnClick a

data Purview a where
  Text :: String -> Purview a
  Html :: String -> [Attribute a] -> Purview a -> Purview a
  State :: state -> ((state, state -> ()) -> Purview a) -> Purview a
  Handler
    :: (Typeable action)
    => (action -> ())
    -> ((action -> ()) -> Purview a)
    -> Purview a
  -- Handler
  -- OnChange
  -- Once

data Action = Up | Down

x send = Html "div" [OnClick (send Up)] (Text "")

handle (state, setState) = Handler handler x
  where handler Up = setState Down
        handler Down = setState Up

comb' = State Up handle

comp (state, setState) = Html "div" [OnClick (setState (state + 1))] (Text "hello")

state = State (0 :: Integer)

comb = state comp
