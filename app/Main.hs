{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           GHC.Generics
import           Lib

data Action
  = Increment
  | Decrement
  deriving (Show, Generic)

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
