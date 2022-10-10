module EventOrdering where

import Prelude hiding (div)
import Control.Concurrent
import Purview


{-

This is to test that running the new state function is happening at the
correct time.

To run the test:
1. Hit "slow"
2. Hit "fast"

Counter should increment immediately after hitting "fast", and 5 seconds
later the count should be increased by 1 when "slow" completes.  The bug
would be the count is overwritten by "slow" completing.

-}

reducer "fast" state = pure (\state -> state + 1, [])
reducer "slow" state = do
  threadDelay (5 * 1000000)
  pure (\state -> state + 1, [])

handler = effectHandler (0 :: Int) reducer

checkButtonSlow :: Purview String m
checkButtonSlow = onClick ("slow" :: String) $ div [ text "slow" ]

checkButtonFast :: Purview String m
checkButtonFast = onClick ("fast" :: String) $ div [ text "fast" ]

combined = handler $ \state -> div
  [ p [ text (show state) ]
  , checkButtonSlow
  , checkButtonFast
  ]

main = run defaultConfiguration { component=const combined }
