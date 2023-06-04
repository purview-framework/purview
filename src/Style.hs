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

import Component (Attributes)

-- thanks https://stackoverflow.com/questions/59399050/haskell-making-quasi-quoted-values-strict-evaluated-at-compile-time

-- newtype Style = Style (String, String)
--   deriving (Show, Eq)

style' :: String -> Q Exp
style' css =
  let hashed = show $ hash css
  in [| Style (hashed, css) |]

style :: QuasiQuoter
style = QuasiQuoter
  { quoteDec = error "quoteDec not implemented"
  , quoteType = error "quoteType not implemented"
  , quotePat = error "quotePat not implemented"
  , quoteExp = style'
  }

-- snagged from https://stackoverflow.com/a/9263004/1361890
hash :: String -> Int
hash = foldl' (\h c -> 33 * h `xor` fromEnum c) 5381
