{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           GHC.Generics
import           Lib

-- for server time example
import           Data.Time
import           Data.Aeson
import           Data.Aeson.TH

---------------------
-- Stepper Example --
---------------------

upButton = onClick "up" $ div [ text "up" ]
downButton = onClick "down" $ div [ text "down" ]

handler = MessageHandler 0 action
  where
    action "up" state   = state + 1
    action "down" state = state - 1

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

logger = print

main = run logger (handler counter)

-------------------------
-- Server Time Example --
-------------------------

newtype UpdateTime = Set UTCTime

$(deriveJSON defaultOptions ''UpdateTime)

display :: UTCTime -> Purview a
display time = div [ text (show time) ]

timeHandler = EffectHandler Nothing action
  where
    action (Set time) state = do
      time <- getCurrentTime
      pure $ Just time

-- timeEffect = Effect send
--   where send action = do
--           time <- getCurrentTime
--           action (Set time)

-- so what would trigger the initial go
