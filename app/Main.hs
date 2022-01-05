{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           GHC.Generics
import           Lib

data Action
  = Increment
  | Decrement
  deriving (Show, Generic)

upButton = onClick "" $ div [ text "up" ]
downButton = onClick "" $ div [ text "down" ]

handler = MessageHandler 0 action
  where
    action :: String -> Int
    action "up"    = 1
    action "down"  = -1
    action "click" = -2

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

logger = print

main = run logger (handler counter)
