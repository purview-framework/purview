{-# LANGUAGE GADTs #-}
module Experiment15 where

import Control.Monad.Writer

f :: Integer -> Integer
f a = a + 1

data Test where
  Box :: String -> Test
  Jack :: ((String -> String) -> Test) -> Test

tmp :: (String -> String) -> Test
tmp fn = Box "Hey"

x = tmp id

combo = Jack tmp

type Append = Writer [String] Integer

-- seems I need a writer monad
flux :: Append
flux = do
  tell . pure $ ("when" :: String)
  pure (1 :: Integer)
