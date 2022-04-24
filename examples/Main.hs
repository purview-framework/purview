{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Main where

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

handler = messageHandler (0 :: Int) reducer
  where
    reducer Up   state = (const $ state + 1, [])
    reducer Down state = (const $ state - 1, [])

counter state = div
  [ upButton
  , text $ "count: " <> show state
  , downButton
  ]

view = handler counter

main = Purview.run defaultConfiguration { component=view }
