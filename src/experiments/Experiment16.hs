{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

-- |

module Experiment16 where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.IO.Class
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Monad
import           Control.Concurrent
import Data.Typeable

heck s = liftIO $ void . forkIO $ liftIO s

{-

Ok, what do I want?

I want people to be able to use transformers easily for effects

What haven't I tried?
Rewriting apply in a monadic way (would that even fix anything?)

What are you really saying with running effects at the top?

Let's try this again

-}

data View action m where
  ActionHandler
    :: (Show state, Typeable state, Typeable action)
    => state                         -- initial state
    -> (action -> state -> m state) -- action handler run by system for events
    -> (state -> View action m)        -- continuation
    -> View action m

  Nil :: View action m

instance Show (View a m) where
  show (ActionHandler state _ _) = show state

reducer :: String -> String -> IO String
reducer action state = pure $ state <> "hello"

handler = ActionHandler "goodbye" reducer (const Nil)

{-

Alright, we have that part.  Now we need a tiny version of apply.

-}

apply :: (Typeable a, Monad m, MonadIO m) => View a m -> m (View a m)
apply view = case view of
  ActionHandler state handler cont -> do
    newState <- case cast "" of
      Just val -> handler val state
      Nothing  -> error "fucka"

{-

This part is dead to me, time to refactor purview proper

    _ <- forkIO $ do
      newState <- case cast "" of
        Just val ->
          handler val state
        Nothing  -> error "fucka"

      liftIO $ print "done"
-}

    pure $ ActionHandler newState handler cont

{-

Cool.  Now back to what do you _mean_ when you say you want people to
easily use effect systems?

They should be per handler.  I want each handler to be able to say it
needs specific effects.  And I want all the effects to be able to be
stated at the top of the tree.  This means each part could also be
tested, knowing exactly which effects it uses.

so now we need an effect

-}

data Time r where
  GetTime :: Time String

makeEffect ''Time

runPureTime :: LastMember IO effs => Eff (Time ': effs) a -> Eff effs a
runPureTime = interpretM $ \case
  GetTime -> pure "123"

{-

And a handler that uses that effect

-}

effectReducer :: Member Time effs => String -> String -> Eff effs String
effectReducer action state = getTime

{-

can we use this handler in a test?

-}

test = do
  val <- runM . runPureTime $ effectReducer "" ""
  print $ val == "123"

{-

Super.

And now we plug it into a handler

-}

effectHandler :: Member Time effs => View String (Eff effs)
effectHandler = ActionHandler "goodbye" effectReducer (const Nil)

{-

Alright.  Now we need to make this work with apply.

-}

wonderful = runM . runPureTime $ apply effectHandler

{-

We almost have it.  Now I just need to find a way for IO to get involved for the
void . forkIO.

-}

{-

It looks like the void . forkIO is impossible.

-}

-- data Temp a m where
--   Box :: (String -> m String) -> Temp a m
--
-- data Time r where
--   GetTime :: Time String
--
-- makeEffect ''Time
--
-- -- test :: Member Time effs => Temp a (Eff effs)
-- test = Box (const getTime)
--
-- useIt :: Member Time effs => Temp a (Eff effs) -> Eff effs String
-- useIt (Box thing) = do
--   y <- thing ""
--   pure (y <> "more")
--
-- runPureTime :: Eff (Time ': effs) ~> Eff effs
-- runPureTime = interpret $ \case
--   GetTime -> pure "123"
--
-- -- what = runM . runPureTime $ do
-- --   print ""
-- --   useIt test
-- --   pure ()
--
-- hmm :: Monad m => Temp a m -> m ()
-- hmm (Box action) = do
--   action ""
--   pure ()
--
-- flub = hmm test
--
-- xyz = id
--
-- t = xyz test
--
-- -- func = do
-- --   test
--
-- overlap :: Temp a m -> IO ()
-- overlap x = undefined
--
-- -- applyEvent :: Member Time effs => Temp a (Eff effs) -> IO (Temp a (Eff effs))
-- -- applyEvent view = case view of
-- --   Box fn -> do
-- --     void . forkIO $ do
-- --       fn ""
-- --       pure ()
-- --
-- --     pure (Box fn)
-- --
-- -- heck = applyEvent test
--
-- feh :: IO (String)
-- feh = runM . runPureTime $ useIt test
