{-# LANGUAGE FlexibleInstances #-}
module TreeGenerator where

import Prelude hiding (div)
import Test.Hspec
import Test.QuickCheck hiding (once)
import Control.Monad.IO.Class

import Component
import ComponentHelpers
import Events

{-

This is a helper to generate semi random purview trees
for tests, and to make sure everything can be combined
nicely.

-}

testHandler :: (String -> Purview String IO) -> Purview String IO
testHandler = effectHandler' [] ("" :: String) reducer
  where
    reducer :: String -> String -> IO (String, [DirectedEvent String String])
    reducer action state = pure ("", [])

sizedArbExpr :: Int -> Gen (Purview String IO)
sizedArbExpr 0 = do pure $ text "always present"
sizedArbExpr n = do
  es <- vectorOf 2 (sizedArbExpr (n-1))
  elements
    [ div es
    , istyle "" $ div es
    , testHandler (const $ div es)
    ]

instance Arbitrary (Purview String IO) where
  arbitrary = resize 3 $ sized sizedArbExpr
