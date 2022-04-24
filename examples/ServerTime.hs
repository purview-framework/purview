{-# LANGUAGE TemplateHaskell #-}
module ServerTime where

import Prelude hiding (div)

import Data.Aeson
import Data.Aeson.TH
import Data.Time

import Purview

data UpdateTime = UpdateTime

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''UpdateTime)

display time = div
  [ text (show time)
  , onClick UpdateTime $ div [ text "check time" ]
  ]

startClock cont state = Once temp False (cont state)
 where temp send = do
         send UpdateTime

timeHandler = effectHandler Nothing handle
  where
    handle UpdateTime state = do
      time <- Just <$> getCurrentTime
      pure (const time, [])

main = run defaultConfiguration { component=timeHandler (startClock display) }
