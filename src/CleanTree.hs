{-# LANGUAGE BangPatterns #-}
-- |

module CleanTree where

import Data.Typeable
import Data.List

import Component


{-

I think I can just remove this special handling?

-}
removeClassCSS :: [(Hash, String)] -> Attributes e -> Attributes e
removeClassCSS foundCSS attr = case attr of
  style@Style {} -> style { captured=True }
  _ -> attr

cleanTree :: Typeable event => [(Hash, String)] -> Purview event m -> Purview event m
cleanTree css component = case component of
  Attribute attr cont ->
    let
      tree = cleanTree css cont
      cleanedAttr = removeClassCSS css attr
    in
      Attribute cleanedAttr tree

  Html kind children ->
    let
      cleanChildren = fmap (cleanTree css) children
    in
      Html kind cleanChildren

  EffectHandler ploc loc initEvents state handler cont  ->
    let
      cleanCont = fmap (cleanTree css) cont
    in
      EffectHandler ploc loc [] state handler cleanCont

  Handler ploc loc initEvents state handler cont ->
    let
      cleanCont = fmap (cleanTree css) cont
    in
      Handler ploc loc [] state handler cleanCont

  r@Receiver {} -> r
  t@(Text val)  -> t
  v@(Value val) -> v
