module Rendering where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List (find)
import           Unsafe.Coerce

import           Component

isOn :: Attributes a -> Bool
isOn (OnClick _) = True
isOn _           = False

isGeneric :: Attributes a -> Bool
isGeneric (Generic _ _) = True
isGeneric _ = False

isSubmit :: Attributes a -> Bool
isSubmit (OnSubmit _) = True
isSubmit _            = False

getStyle :: Attributes a -> String
getStyle (Style style') = style'
getStyle _              = ""

renderGeneric :: Attributes a -> String
renderGeneric attr = case attr of
  (Generic name value) -> " " <> name <> "=" <> unpack (encode value)
  _ -> ""

renderAttributes :: [Attributes a] -> String
renderAttributes attrs =
  let styles = concatMap getStyle attrs
      renderStyle = if not (null styles) then " style=" <> show styles else ""

      click = find isOn attrs
      renderClick = case click of
        Just (OnClick action) -> " action=" <> unpack (encode action)
        _                     -> ""

      submit = find isSubmit attrs
      renderSubmit = case submit of
        Just (OnSubmit action) -> " action=" <> unpack (encode action)
        _                      -> ""

      generics = filter isGeneric attrs
      renderedGenerics = concatMap renderGeneric generics
  in
    renderStyle <> renderClick <> renderSubmit <> renderedGenerics

{-|

Takes the tree and turns it into HTML.  Attributes are passed down to children until
they reach a real HTML tag.

-}

render :: Purview a m -> String
render = render' []

render' :: [Attributes a] -> Purview a m -> String
render' attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> renderAttributes attrs <> ">"
    <> concatMap (render' []) rest <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attr rest ->
    render' (attr:attrs) rest

  EffectHandler parentLocation location state _ cont ->
    "<div handler=" <> (show . encode) location <> ">" <>
      render' attrs (cont state) <>
    "</div>"

  Once _ _hasRun cont ->
    render' attrs cont

  Value a -> show a

  Hide a -> render' attrs (unsafeCoerce a)
