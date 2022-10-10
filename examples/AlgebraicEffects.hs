{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module AlgebraicEffects where

import Prelude hiding (div)

import Data.Aeson
import Data.Aeson.TH
import Data.Time

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.IO.Class

import Purview

type Id a = a -> a

{-

Creating a simple effect

-}

data Time r where
  GetTime :: Time String

makeEffect ''Time

{-

With a really basic interpreter

-}

runPureTime :: Eff (Time ': effs) ~> Eff effs
runPureTime = interpret $ \case
  GetTime -> pure "10:10 am"

runIOTime :: LastMember IO effs => Eff (Time ': effs) a -> Eff effs a
runIOTime = interpretM $ \case
  GetTime -> show <$> getCurrentTime

{-

The allowed actions for the reducer and components

-}

data UpdateTime = UpdateTime
  deriving Eq

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''UpdateTime)

{-

The reducer that uses the effect

-}

reducer
  :: Member Time effs
  => UpdateTime -> Maybe String -> Eff effs (Id (Maybe String), [DirectedEvent () UpdateTime])
reducer action state = do
  time <- getTime
  pure (const $ Just time, [])

{-

Example of a tiny test for the reducer

-}

test = do
  let currentState = Just "currentState"

  (newStateFn, actions) <- runM . runPureTime $ reducer UpdateTime (Just "current state")

  print $ newStateFn currentState == Just "10:10 am"

{-

The display component, handler, and combined component

-}

display time = div
  [ text (show time)
  , onClick UpdateTime $ div [ text "check time" ]
  ]

timeHandler = effectHandler Nothing reducer -- tie the reducer in with initial state of "Nothing"

view = timeHandler display

{-

Putting it all together

-}

main = Purview.run defaultConfiguration { component=const view, interpreter=runM . runIOTime }
