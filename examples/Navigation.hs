{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Navigation where

import Prelude hiding (div)
import Purview

router BrowserInformation { path } = case path of
  "/a" -> div [ text "a" ]
  "/b" -> div [ text "b" ]
  _    -> div [ text "default" ]

main = Purview.run defaultConfiguration { component=router }
