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

handler = MessageHandler (0 :: Int) action
  where
    action "up" state   = state + 1
    action "down" state = state - 1

counter :: Int -> Purview a
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

logger = print

-- main = run logger (handler counter)

-------------------------
-- Server Time Example --
-------------------------

data UpdateTime = UpdateTime

$(deriveJSON defaultOptions ''UpdateTime)

display :: Maybe UTCTime -> Purview a
display time = div
  [ text (show time)
  , onClick "setTime" $ div [ text "check time" ]
  ]

startClock cont state = Once (\send -> send "setTime") False (cont state)

timeHandler = EffectHandler Nothing handle
  where
    handle "setTime" state = do
      time <- getCurrentTime
      pure $ Just time
    handle _ state = pure state

main = run logger (timeHandler (startClock display))

-- timeEffect = Effect send
--   where send action = do
--           time <- getCurrentTime
--           action (Set time)

-- so what would trigger the initial go
