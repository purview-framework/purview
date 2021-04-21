{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import Data.Text hiding (count)
import Lib

newtype State = State
  { count :: Int } deriving Show

defaultCounterState = State { count = 0 }

handlers' :: State -> Text -> State
handlers' state message = case message of
  "increment" -> state { count = count state + 1 }
  "decrement" -> state { count = count state - 1 }

render' :: State -> Html
render' state =
  div []
    [ div [ onClick "increment" ] [ text "increment" ]
    , text ("count: " <> show (count state))
    , div [ onClick "decrement" ] [ text "decrement" ]
    ]

counter = Component
  { state = defaultCounterState
  , handlers = handlers'
  , render = render'
  }

main = run counter
