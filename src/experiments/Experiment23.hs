{-# LANGUAGE GADTs #-}
module Experiment23 where

data Tree a where
  Node :: a -> [Tree a] -> Tree a
  Parent :: (action -> [Either action newAction]) -> [Tree ]
