{-# LANGUAGE TemplateHaskell #-}
module Counter where

import           Prelude      hiding (div)

import           Data.Aeson
import           Data.Aeson.TH

import           Purview


---------------------
-- Stepper Example --
---------------------

data Direction = Up | Down

$(deriveJSON defaultOptions ''Direction)

upButton = onClick Up $ div [ text "up" ]
downButton = onClick Down $ div [ text "down" ]

handler :: Applicative m => (Int -> Purview Direction m) -> Purview () m
handler = effectHandler (0 :: Int) reducer
  where
    reducer Up   state = pure (const $ state + 1, [])
    reducer Down state = pure (const $ state - 1, [])

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

view = handler counter
--
main = Purview.run defaultConfiguration { component=view }
