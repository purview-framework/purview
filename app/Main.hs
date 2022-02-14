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

data Direction = Up | Down

$(deriveJSON defaultOptions ''Direction)

upButton = onClick Up $ div [ text "up" ]
downButton = onClick Down $ div [ text "down" ]

handler = messageHandler (0 :: Int) action
  where
    action Up state   = state + 1
    action Down state = state - 1

counter :: Int -> Purview a
counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

component = handler counter

multiCounter = div
  [ component
  , component
  ]

logger = print

main = run logger multiCounter


-------------------------
-- Server Time Example --
-------------------------

data UpdateTime = UpdateTime

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''UpdateTime)

display :: Maybe UTCTime -> Purview a
display time = div
  [ text (show time)
  , onClick UpdateTime $ div [ text "check time" ]
  ]

startClock cont state = Once (\send -> send UpdateTime) False (cont state)

timeHandler = effectHandler Nothing handle
  where
    handle UpdateTime state = Just <$> getCurrentTime

-- main = run logger (timeHandler (startClock display))
