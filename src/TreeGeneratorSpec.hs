{-# LANGUAGE FlexibleInstances #-}
module TreeGeneratorSpec where

import Prelude hiding (div)
import Test.QuickCheck hiding (once)

import Component
import Events

data TestPurviewT a m = Purview a m

testHandler :: (String -> Purview String IO) -> Purview a IO
testHandler = effectHandler ("" :: String) reducer
  where
    reducer :: String -> String -> IO (String, [DirectedEvent String String])
    reducer action state = pure ("", [])

testOnce = once (\send -> send "")

sizedArbExpr :: Int -> Gen (Purview String IO)
sizedArbExpr 0 = do pure $ text "always present"
sizedArbExpr n = do
  es <- vectorOf 2 (sizedArbExpr (n-1))
  elements
    [ div es
    , style "" $ div es
    , testHandler (const $ div es)
    , testOnce (div es)
    ]

instance Arbitrary (Purview String IO) where
  arbitrary = resize 3 $ sized sizedArbExpr
