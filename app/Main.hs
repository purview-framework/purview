{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Lib

data State a b = State
  { state :: a
  , other :: b
  }

todo :: State (Maybe String) (Either String String)
todo = State
  { state = Just "123"
  , other = Left "aaa"
  }

another :: State (Either String String) (Maybe String)
another = State
  { state = Right "aa"
  , other = Nothing
  }

newtype Component state = Component
  { render :: state -> String }

newtype SmallState = SmallState
  { name :: String }

data BigState = BigState
  { address :: String
  , user :: SmallState
  }

smallComponent :: Component SmallState
smallComponent = Component
  { render = \state -> "" }


todoComponent :: Component BigState
todoComponent = Component
  {
    render = \state -> "" <> render smallComponent (user state)
  }

-- class Stateable a b | c where
--   sstate :: c
--   sother :: c

main :: IO ()
main = undefined
