{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           GHC.Generics
import           Purview

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
  , onClick UpdateTime $ div [ text "check time" ]
  ]

startClock cont state = Once (\send -> send UpdateTime) False (cont state)

timeHandler = EffectHandler Nothing handle
  where
    handle UpdateTime state = Just <$> getCurrentTime
    handle _ state = pure state

main = run logger (timeHandler (startClock display))
