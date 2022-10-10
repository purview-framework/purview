{-# LANGUAGE TemplateHaskell #-}
module ExpandingList where

import Prelude hiding (div)

import Data.Aeson
import Data.Aeson.TH

import Purview

data Operation = AddItem | RemoveItem

$(deriveJSON defaultOptions ''Operation)

handler = simpleHandler ([] :: [String]) action
  where
    action AddItem state = "item" : state
    action RemoveItem (x:xs) = xs
    action RemoveItem [] = []

display items = div
  [ div $ fmap (\item -> div [ text item ]) items
  , onClick AddItem (div [ text "add" ])
  , onClick RemoveItem (div [ text "remove" ])
  ]

main = run defaultConfiguration { component=const (handler display) }
