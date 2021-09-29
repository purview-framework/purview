{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           Data.Text    hiding (count)
import           Data.Aeson
import           GHC.Generics
import           Debug.Trace
import           Lib

newtype State = State
  { count :: Int } deriving Show

defaultCounterState = State { count = 0 }

data Action
  = Increment
  | Decrement
  deriving (Show, Read, Generic)

instance FromJSON Action where

handlers' :: State -> Action -> State
handlers' state message = case message of
  Increment -> state { count = count state + 1 }
  Decrement -> state { count = count state - 1 }

other :: Component String Action
other = Component
  { state = "blank"
  , handlers = \state message -> case message of
      Increment -> "incremented"
      Decrement -> "decremented"
  , render = \state -> div [] [ text state ]
  }

feh = SomeComponent other

render' :: State -> Html Action
render' state =
  div [ style "font-size: 48px;" ]
    [ div [ onClick Increment ] [ text "increment" ]
    , text ("count: " <> show (count state))
    , div [ onClick Decrement ] [ text "decrement" ]
    , feh
    ]

counter = MkSomeComponent $ Component
  { state    = defaultCounterState
  , handlers = handlers'
  , render   = render'
  }

logger = print

main = run logger counter

-- so we can pass in the logger
main' = flip run counter
