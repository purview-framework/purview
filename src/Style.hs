{-# LANGUAGE TemplateHaskell #-}

-- |
module Style
  ( style
  , handleCSS
  , parseLine
  , parseCSS
  , Brace (..)
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

{-

This is all kind of a mess and could definitely use some love. I don't think
it's quite doing what is wanted, looking at the dumped splices, as ideally
that would be a list of Attributes instead of a call to a function.  I ran
into implementing Lift for Purview -> Purview, for the attributes, and this
at least works.

If it catches anyone's eye by all means rewrite it

-}
clean :: String -> String
clean []          = ""
clean ('\n':rest) = clean rest
clean (';':rest)  = ';':clean (dropWhile (flip elem [' ', '\n']) rest)
clean (c:rest)    = c:clean rest

data Brace = Open | Close | None
  deriving (Show, Eq)

-- Takes a chunk of CSS and returns a line, the remainder to parse, and if
-- it contains an opening brace
parseLine' :: (String, String) -> (Brace, String, String)
parseLine' (line, '\n':rest) = parseLine' (line, rest)
parseLine' (line, ';':rest)  = (None, line <> [';'], rest)
parseLine' (line, '{':rest)  = (Open, line <> ['{'], rest)
parseLine' (line, '}':rest)  = (Close, line <> ['{'], rest)
parseLine' (line, c:rest)    = parseLine' (line <> [c], rest)
parseLine' (line, "")        = (None, line, "")

preParseLine :: String -> String
preParseLine = dropWhile (flip elem [' ', '\n'])

parseLine css =
  let cleaned = preParseLine css
  in parseLine' ("", cleaned)

parseNewLevel = takeWhile (/= '{')

parseCSS :: [String] -> String -> [(String, String)]
parseCSS _ ""     = []
parseCSS path css =
  let (brace, line, remaining) = parseLine css
  in case brace of
    Open  -> parseCSS (path <> [parseNewLevel line]) remaining
    Close -> parseCSS (take (length path - 1) path) remaining
    None  ->
      if null line
      then []
      else (concat path, line) : parseCSS path remaining

joinOnClass :: [(String, String)] -> (String, String)
joinOnClass cs@((name, _):_) = (name, concatMap snd cs)
joinOnClass [] = ("", "")

handleCSS :: String -> [(String, String)]
handleCSS css =
  fmap joinOnClass $ groupBy (\a b -> fst a == fst b) $ sortOn fst $ parseCSS [] css

toAttributes :: String -> String -> (Purview event m -> Purview event m)
toAttributes hashed css =
  let ((_, baseCss):rest) = handleCSS css
  in foldr
      (\(newClass, newCss) acc -> acc . Attribute (Style (hashed <> " " <> newClass, newCss)))
      (Attribute (Style (hashed, baseCss))) rest

style' :: String -> Q Exp
style' css =
  -- pretty funny, css needs a leading character (not number)
  let
    hashed = 'p' : show (hash css)
  in [| toAttributes hashed css |]

-- snagged from https://stackoverflow.com/a/9263004/1361890
hash :: String -> Int
hash = foldl' (\h c -> 33 * h `xor` fromEnum c) 5381
