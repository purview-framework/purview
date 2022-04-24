{-# LANGUAGE TemplateHaskell #-}
module GettingInput where

import Prelude hiding (div)

import Data.Aeson
import Data.Aeson.TH

import Purview

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"

data Fields = Fields
  { textField :: String
  }

$(deriveJSON defaultOptions ''Fields)

handler = simpleHandler "" action
  where
    action (Fields txt) _ = txt

submitButton = typeAttr "submit" $ button [ text "submit" ]

defaultFields = Fields { textField="" }

display txt = div
  [ text ("you submitted: " <> txt)
  , onSubmit defaultFields $ form
    [ nameAttr "textField" $ input []
    , submitButton
    ]
  ]

main = run defaultConfiguration { component=handler display }
