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

isClass :: Attributes a -> Bool
isClass (Generic "class" _) = True
isClass _                   = False

isInlineStyle :: Attributes a -> Bool
isInlineStyle (Generic "style" _) = True
isInlineStyle _                   = False

getInlineStyle :: Attributes a -> String
getInlineStyle (Generic "style" css) = css
getInlineStyle _                     = ""

getClassBasedStyle :: Attributes a -> String
getClassBasedStyle (Style { hash, css }) =
  -- filter out things like "p123 li", which are created
  -- by nested rules in [style||] templates
  if ' ' `notElem` hash then hash else ""
getClassBasedStyle _ = ""

renderGeneric :: Attributes a -> String
renderGeneric attr = case attr of
  (Generic name value) -> " " <> name <> "=" <> unpack (encode value)
  _                    -> ""

renderAttributes :: [Attributes a] -> String
renderAttributes attrs =
  let
    -- doesn't work like this anymore
    styles = concatMap getInlineStyle attrs
    renderedStyle = if not (null styles) then " style=" <> show styles else ""

    -- TODO: this is uggo
    classStyles = filter (not . null) $ fmap getClassBasedStyle attrs
    existingClasses = (\(Generic _ name) -> name) <$> filter isClass attrs
    combinedClasses = classStyles <> existingClasses

    renderedClasses =
      if not (null combinedClasses)
      then " class=\"" <> unwords combinedClasses <> "\""
      else ""

    listeners = filter isOn attrs
    renderedListeners = concatMap
      (\(On name ident action) -> " " <> name <> "-location=" <> unpack (encode ident))
      listeners
    noticeToBind = if null listeners then "" else " bubbling-bound"

    generics = filter (not . (\v -> isClass v || isInlineStyle v)) $ filter isGeneric attrs
    renderedGenerics = concatMap renderGeneric generics
  in
    renderedStyle <> noticeToBind <> renderedListeners <> renderedGenerics <> renderedClasses

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
    -- collecting all the attributes till we hit html
    render' (attr:attrs) rest

  EffectHandler parentLocation location initEvents state _ cont ->
    "<div handler=" <> (show . encode) location <> ">" <>
      render' attrs (unsafeCoerce cont state) <>
    "</div>"

  Handler { identifier, state, continuation } ->
    "<div handler=" <> (show . encode) identifier <> ">" <>
      render' attrs (unsafeCoerce continuation state) <>
    "</div>"

  Receiver { parentIdentifier, identifier, name, child, state } ->
    "<div" <>
    " handler=" <> (show . encode) identifier <>
    " parent-handler=" <> (show . encode) parentIdentifier <>
    " receiver-name=\"" <> name <> "\"" <>
    ">" <>
      render' attrs (child state) <>
    "</div>"

  Value a -> show a
