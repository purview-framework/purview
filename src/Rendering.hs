{-# LANGUAGE NamedFieldPuns #-}
module Rendering where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (unpack)
import           Unsafe.Coerce

import           Component

isOn :: Attributes a -> Bool
isOn (On _ _ _) = True
isOn _          = False

isGeneric :: Attributes a -> Bool
isGeneric (Generic _ _) = True
isGeneric _             = False

getStyle :: Attributes a -> String
getStyle (Style style') = style'
getStyle _              = ""

renderGeneric :: Attributes a -> String
renderGeneric attr = case attr of
  (Generic name value) -> " " <> name <> "=" <> unpack (encode value)
  _                    -> ""

renderAttributes :: [Attributes a] -> String
renderAttributes attrs =
  let
    styles = concatMap getStyle attrs
    renderedStyle = if not (null styles) then " style=" <> show styles else ""

    listeners = filter isOn attrs
    renderedListeners = concatMap
      (\(On name ident action) -> " location=" <> (unpack $ encode ident))
      listeners

    generics = filter isGeneric attrs
    renderedGenerics = concatMap renderGeneric generics
  in
    renderedStyle <> renderedListeners <> renderedGenerics

{-|

Takes the tree and turns it into HTML.  Attributes are passed down to children until
they reach a real HTML tag.

-}

render :: Purview action m -> String
render = render' []

render' :: [Attributes action] -> Purview action m -> String
render' attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concatMap (render' []) rest <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr rest ->
    render' (attr:attrs) rest

  EffectHandler parentLocation location initEvents state _ cont ->
    "<div handler=" <> (show . encode) location <> ">" <>
      render' attrs (unsafeCoerce cont state) <>
    "</div>"

  Handler { identifier, state, continuation } ->
    "<div handler=" <> (show . encode) identifier <> ">" <>
      render' attrs (unsafeCoerce continuation state) <>
    "</div>"

  Value a -> show a
