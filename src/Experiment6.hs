{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Experiment6 where

-- the only goal is to figure out onClick

data Attr = OnClick (String)

data Html
  = Html String [ Attr ] [ Html ]
  | Text String

component = Html "button" [ OnClick "hey" ] [ Text "click" ]

-- I wonder if I already sort of have the answer to this?
