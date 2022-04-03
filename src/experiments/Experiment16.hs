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

data Temp a m where
  Box :: String -> m String -> Temp a m

data Time r where
  GetTime :: Time String

makeEffect ''Time

test :: Member Time effs => Temp a (Eff effs)
test = Box "" getTime

useIt :: Member Time effs => Temp a (Eff effs) -> Eff effs String
useIt (Box _ thing) = do
  y <- thing
  pure (y <> "more")

runPureTime :: Eff (Time ': effs) ~> Eff effs
runPureTime = interpret $ \case
  GetTime -> pure "123"

feh = run . runPureTime $ useIt test
