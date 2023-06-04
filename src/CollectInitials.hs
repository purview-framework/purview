{-# LANGUAGE DuplicateRecordFields #-}
-- |

module CollectInitials where

import Data.Typeable

import Component
import Events

import Debug.Trace

type Location = [Int]

getStyleFromAttr :: Attributes e -> Maybe (Hash, String)
getStyleFromAttr attr = case attr of
  Style (hash, css) ->
    if hash /= "-1" && css /= ""
    then Just (hash, css)  -- set the css to empty since it's been caught
    else Nothing
  _ -> Nothing

directedEventToInternalEvent :: (Typeable a, Typeable b) => Location -> Location -> DirectedEvent a b -> Event
directedEventToInternalEvent parentLocation location directedEvent = case directedEvent of
  Parent event -> InternalEvent { event=event, childId=Nothing, handlerId=Just parentLocation }
  Self event   -> InternalEvent { event=event, childId=Nothing, handlerId=Just location }
  Browser {}   -> error "tried to turn a browser event into an internal event"

collectInitials :: Typeable event => Purview event m -> ([Event], [(Hash, String)])
collectInitials component = case component of
  Attribute attr cont ->
    let
      (events, css) = collectInitials cont
      possibleCss = getStyleFromAttr attr
    in
      case possibleCss of
        Just newCss -> (events, newCss : css)
        Nothing     -> (events, css)

  Html kind children ->
    let
      eventsAndCss = fmap collectInitials children
      events = concatMap fst eventsAndCss
      css = concatMap snd eventsAndCss
    in
      (events, css)

  EffectHandler ploc loc initEvents state _handler cont  ->
    let
      (events, css) = collectInitials (cont state)
      internalizedEvents = case (ploc, loc) of
        (Just ploc, Just loc) -> fmap (directedEventToInternalEvent ploc loc) initEvents
        _                     -> error "EffectHandler missing locations"
    in
      (internalizedEvents <> events, css)

  Handler ploc loc initEvents state _handler cont ->
    let
      (events, css) = collectInitials (cont state)
      internalizedEvents = case (ploc, loc) of
        (Just ploc, Just loc) -> fmap (directedEventToInternalEvent ploc loc) initEvents
        _                     -> error "Handler missing locations"
    in
      (internalizedEvents <> events, css)

  Receiver {} -> ([], [])
  Text val    -> ([], [])
  Value val   -> ([], [])
