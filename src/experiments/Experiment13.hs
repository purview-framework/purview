{-# LANGUAGE GADTs #-}
-- |

module Experiment13 where

import Data.Typeable

{-

Maybe I should try a new technique that isn't based entirely
around a record

-}

data Purview where
  Html :: String -> Purview
  Handler :: (Typeable messages) => state -> (messages -> state) -> (state -> Purview) -> Purview

t = Html "a"

x state = Html (show state)

data Actions = Up | Down
  deriving Show

y = Handler Up handle x
  where
    handle Up = Down
    handle Down = Up


walk :: Actions -> Purview -> String
walk action (Handler st handle rest) =
  let newSt = case cast action of
        Just t -> handle t
        Nothing -> st
  in walk action (rest newSt)
walk _ (Html con) = con
