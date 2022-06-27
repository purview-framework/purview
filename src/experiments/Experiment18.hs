{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- |

module Experiment18 where

import Data.Aeson

data Component parentAction action m where
  Handler ::
    { initialState :: FromJSON state => state
    , handler ::
        ( FromJSON parentAction
        , FromJSON action
        )
        => action
        -> state
        -> (state, Either parentAction action)
    , continuation :: state -> Component parentAction action m
    }
    -> Component parentAction action m

  Nil :: a -> Component parentAction action m

  Hide :: Component parentAction action m -> Component action any m

  -- Test :: FromJSON b => (b -> Either action b) -> Component b m

temp = Handler
  ""
  (\(action :: Int) state -> (state, Left (1 :: Integer)))
  Nil

parent = Handler
  ""
  (\(action :: String) state -> (state, Right ""))

son = Handler
  ""
  (\(action :: Int) state -> (state, Left (1 :: Int)))

combine = parent $ const $ son Nil

-- temp2 = Test (\(something) -> Left 1)

{-

Alright so it seems that changing the construction of handlers to the
record style is a step forward

-}
