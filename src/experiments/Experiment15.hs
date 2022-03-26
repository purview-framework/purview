{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Experiment15 where

import Prelude hiding (log)
import Control.Monad.Writer
import Purview

f :: Integer -> Integer
f a = a + 1

-- data Test where
--   Box :: String -> Test
--   Jack :: ((String -> String) -> Test) -> Test

-- tmp :: (String -> String) -> Test
-- tmp fn = Box "Hey"
--
-- x = tmp id
--
-- combo = Jack tmp
--
-- type Append = Writer [String] Integer
--
-- -- seems I need a writer monad
-- flux :: Append
-- flux = do
--   tell . pure $ ("when" :: String)
--   pure (1 :: Integer)

-- data Another where
--   Example :: String -> (String -> Writer [String] Integer) -> Another

data Another where
  Example :: String -> (String -> Integer) -> Another

fun :: Another -> Writer [String] Another
fun (Example state fn) = do
  tell $ pure "hey"
  pure $ Example "state" (const 1)

func :: (WriterT [String] IO String)
func = do
  tell $ pure "hey"
  liftIO $ print "sup"
  pure $ ""

log :: (Monad m, MonadTrans t, Monoid w) => w -> t (WriterT w m) ()
log = lift . tell

wumbo :: (Writer [String] (Purview a))
wumbo = do
  tell (pure "hey")
  -- lift . tell ["hallo"]
  -- liftIO $ print "sup"
  pure (text "")

  -- pure $ fmap tell ""
  -- pure . lift $ pure ""

-- funcb :: IO (Writer [String] String)
-- funcb = tell "hell" >>= \x -> pure x
