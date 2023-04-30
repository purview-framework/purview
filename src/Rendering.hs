{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rendering where

import Prelude hiding (concatMap, concat, null)

import           Data.Aeson
-- import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.ByteString.Lazy (ByteString, unpack, concat, null, unpack)
import           Unsafe.Coerce

import           Component
import Data.Aeson.Encoding (encodingToLazyByteString)

isOn :: Attributes a -> Bool
isOn On {} = True
isOn _     = False

isGeneric :: Attributes a -> Bool
isGeneric (Generic _ _) = True
isGeneric _             = False

getStyle :: Attributes a -> ByteString
getStyle (Style style') = style'
getStyle _              = ""

renderGeneric :: Attributes a -> ByteString
renderGeneric attr = case attr of
  (Generic name value) -> " " <> name <> "=" <> value
  _                    -> ""

renderAttributes :: [Attributes a] -> ByteString
renderAttributes attrs =
  let
    styles :: ByteString
    styles = concat $ fmap getStyle attrs
    renderedStyle = if not (null styles) then " style=" <> styles else ""

    listeners = filter isOn attrs
    renderedListeners = concat $ fmap
      (\(On name ident action) -> " " <> name <> "-location=" <> encode ident)
      listeners
    noticeToBind = if length listeners == 0 then "" else " bubbling-bound"

    generics = filter isGeneric attrs
    renderedGenerics = concat $ fmap renderGeneric generics
  in
    renderedStyle <> noticeToBind <> renderedListeners <> renderedGenerics

{-|

Takes the tree and turns it into HTML.  Attributes are passed down to children until
they reach a real HTML tag.

-}

render :: Purview action m -> ByteString
render = render' []

render' :: [Attributes action] -> Purview action m -> ByteString
render' attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concat (fmap (render' []) rest) <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr rest ->
    render' (attr:attrs) rest

  EffectHandler parentLocation location initEvents state _ cont ->
    "<div handler=" <> encode location <> ">" <>
      render' attrs (unsafeCoerce cont state) <>
    "</div>"

  Handler { identifier, state, continuation } ->
    "<div handler=" <> encode identifier <> ">" <>
      render' attrs (unsafeCoerce continuation state) <>
    "</div>"

  Value a -> undefined -- unpack (show a)
