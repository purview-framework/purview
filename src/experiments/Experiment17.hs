{-# LANGUAGE GADTs #-}
module Experiment17 where

import Data.Aeson


{-

Time to chat w urself

Can this be done with only the parent action?

-}
data Box parentAction newAction m where
  Make
    :: (FromJSON parentAction, FromJSON newAction)
    => state
    -> (newAction -> state -> [Either parentAction newAction])
    -> (state -> Box newAction any m)
    -> Box parentAction newAction m

  Nil :: Box parentAction newAction m

  Hide :: Box parentAction newAction m -> Box parentAction any m


top = Make "f" reducer
  where
    reducer 0 state = [Right 0] :: [Either String Integer]

middle = Make "f" reducer
  where
    reducer "" state = [] :: [Either String String]

bottom = Make (1 :: Integer) reducer
  where
{-

Let's focus on this.  The type is simply wrong.  I am wrong.  The type is correct.

-}
    reducer "" 0 = []
    -- reducer "" 0 = [Left "String", Right "herrr"]

topHidden = Hide . top

bottomHidden = Hide . bottom

middleHidden = Hide . middle

x = [topHidden $ const Nil, middleHidden $ const Nil]

combo = topHidden $ const (bottomHidden $ const Nil)

-- both =
--   [ topHidden $ const Nil
--   , bottomHidden $ const Nil
--   ]

-- data  Box parentAction action m where
--   Make
--     :: (FromJSON parentAction, FromJSON action)
--     => state
--     -> (action -> state -> [Either parentAction action])
--     -> (state -> Box action c m)
--     -> Box action b m
--
--   Hide :: Box parentAction b m -> Box b c m
--
--   Nil :: Box parentAction b m
--
-- top = Make "f" reducer
--   where
--     reducer 0 state = [Right 0] :: [Either String Integer]
--
-- bottom = Make 1 reducer
--   where
--     reducer "" 0 = [Left (1 :: Integer)]
--
-- topHidden = top
--
-- bottomHidden = bottom
--
-- both =
--   [ topHidden $ const Nil
--   , bottomHidden $ const Nil
--   ]
--
-- combo = topHidden $ const (bottomHidden $ const Nil)
