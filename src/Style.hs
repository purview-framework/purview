{-# LANGUAGE TemplateHaskell #-}

-- |
module Style
  ( style
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Bits
import Data.List

import Component (Attributes ( Style ), Purview ( Attribute ))

-- thanks https://stackoverflow.com/questions/59399050/haskell-making-quasi-quoted-values-strict-evaluated-at-compile-time

style :: QuasiQuoter
style = QuasiQuoter
  { quoteDec = error "quoteDec not implemented"
  , quoteType = error "quoteType not implemented"
  , quotePat = error "quotePat not implemented"
  , quoteExp = style'
  }

style' :: String -> Q Exp
style' css =
  -- pretty funny, css needs a leading character (not number)
  let hashed = 'p' : show (hash css)
  in [| Attribute (Style (hashed, css)) |]


-- snagged from https://stackoverflow.com/a/9263004/1361890
hash :: String -> Int
hash = foldl' (\h c -> 33 * h `xor` fromEnum c) 5381
