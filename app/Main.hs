{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude      hiding (div)
import           Data.Text    hiding (count)
import           Lib

newtype State = State
  { count :: Int } deriving Show

defaultCounterState = State { count = 0 }

data Action
  = Increment
  | Decrement
  deriving (Show, Read)

handlers' :: State -> Action -> State
handlers' state message = case message of
  Increment -> state { count = count state + 1 }
  Decrement -> state { count = count state - 1 }

render' :: State -> Html Action
render' state =
  div [ style "font-size: 48px;" ]
    [ div [ onClick Increment ] [ text "increment" ]
    , text ("count: " <> show (count state))
    , div [ onClick Decrement ] [ text "decrement" ]
    ]

counter = Component
  { state    = defaultCounterState
  , handlers = handlers'
  , render   = render'
  }

logger = print

main = run logger counter

-- so we can pass in the logger
main' = flip run counter
