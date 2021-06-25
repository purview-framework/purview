{-# LANGUAGE OverloadedStrings #-}
-- | Can we do an add to cart button that loads

module Shop where

import           Prelude      hiding (div)
import           Data.Text    hiding (count)
import           Lib

-- newtype State = State
--   { shop :: Shop
--   , cart :: Cart
--   } deriving Show

-- it goes like:
-- click add to cart
-- -> show add to cart as loading
-- -> cart state is updated
-- -> call show cart
