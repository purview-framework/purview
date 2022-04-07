{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Aeson
import           Data.List (find)
import           Data.Typeable
import           Unsafe.Coerce
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Monad
import           Control.Concurrent

import           Events

data Attributes action where
  OnClick :: ToJSON action => action -> Attributes action
  OnSubmit :: ToJSON action => action -> Attributes action
  Style :: String -> Attributes action
  Generic :: String -> String -> Attributes action

type Identifier = Maybe [Int]
type ParentIdentifier = Identifier

data Purview a m where
  Attribute :: Attributes a -> Purview a m -> Purview a m
  Text :: String -> Purview a m
  Html :: String -> [Purview a m] -> Purview a m
  Value :: Show a => a -> Purview a m

  EffectHandler
    :: (FromJSON action, FromJSON state, ToJSON action, ToJSON a, ToJSON state, Typeable state, Eq state)
    => ParentIdentifier
    -> Identifier
    -> state
    -> (action -> state -> m (state, [DirectedEvent a action]))
    -> (state -> Purview action m)
    -> Purview action m

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview a m
    -> Purview a m

  Hide :: Purview a m -> Purview b m

instance Show (Purview a m) where
  show (EffectHandler parentLocation location state _action cont) =
    "EffectHandler " <> show parentLocation <> " " <> show location <> " " <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute _attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value
  show (Hide a) = "Hide " <> show a

instance Eq (Purview a m) where
  a == b = show a == show b

-- Various helpers
div :: [Purview a m] -> Purview a m
div = Html "div"

form :: [Purview a m] -> Purview a m
form = Html "form"

text :: String -> Purview a m
text = Text

style :: String -> Purview a m -> Purview a m
style = Attribute . Style

onClick :: ToJSON b => b -> Purview b m -> Purview b m
onClick = Attribute . OnClick

onSubmit :: ToJSON b => b -> Purview b m -> Purview b m
onSubmit = Attribute . OnSubmit

identifier :: String -> Purview a m -> Purview a m
identifier = Attribute . Generic "id"

classes :: [String] -> Purview a m -> Purview a m
classes xs = Attribute . Generic "class" $ unwords xs

-- effectHandler
--   :: (FromJSON action, FromJSON state, ToJSON action, ToJSON parent, ToJSON state, Typeable state, Eq state)
--   => state
--   -> (action -> state -> IO (state, [DirectedEvent parent action]))
--   -> (state -> Purview action)
--   -> Purview a
effectHandler state handler =
  Hide . EffectHandler Nothing Nothing state handler

messageHandler state handler = effectHandler state (\action state -> pure (handler action state))

getStyle :: Attributes a -> String
getStyle (Style style') = style'
getStyle _              = ""

isOn :: Attributes a -> Bool
isOn (OnClick _) = True
isOn _           = False

isGeneric :: Attributes a -> Bool
isGeneric (Generic _ _) = True
isGeneric _ = False

isSubmit :: Attributes a -> Bool
isSubmit (OnSubmit _) = True
isSubmit _            = False

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

{-|

This is a special case event to assign state to message handlers

-}

applyNewState :: TChan FromEvent -> FromEvent -> Purview a m -> Purview a m
applyNewState eventBus fromEvent@FromEvent { message, location } component = case component of
  EffectHandler ploc loc state handler cont -> case fromJSON message of
    Success newState -> do
      if loc == location
        then EffectHandler ploc loc newState handler cont
        -- TODO: continue down the tree
        else EffectHandler ploc loc state handler cont
    Error _ -> do
      EffectHandler ploc loc state handler cont

  Hide x ->
    let
      children = applyNewState eventBus fromEvent x
    in
      Hide children

  -- TODO: continue down the tree
  x -> x

runEvent :: Monad m => FromEvent -> Purview a m -> m [FromEvent]
runEvent fromEvent@FromEvent { message, location } component = case component of
  EffectHandler parentLocation loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      -- if locations match, we actually run what is in the handler
      (newState, events) <-
        if loc == location
        then handler parsedAction state
        else pure (state, [])

      -- although it doesn't break anything, only send this when the
      -- locations match (cuts down on noise)
      let newStateEvent =
            if loc == location then
              [
                FromEvent
                { event = "newState"
                , message = toJSON newState
                , location = loc
                }
              ]
            else
              []

      let createMessage directedEvent = case directedEvent of
            (Parent event) -> FromEvent
              { event = "internal"
              , message = toJSON event
              , location = parentLocation
              }
            (Self event) -> FromEvent
              { event = "internal"
              , message = toJSON event
              , location = loc
              }

      -- here we handle sending events returned to either this
      -- same handler or passing it up the chain
      -- mapM_ (atomically . writeTChan eventBus . createMessage) events
      let handlerEvents = fmap createMessage events

      -- ok, right, no where in this function does the tree actually change
      -- that's handled by the setting state event
      childEvents <- runEvent fromEvent (cont state)

      -- so we can ignore the results from applyEvent and continue
      -- pure $ EffectHandler parentLocation loc state handler cont
      pure $ newStateEvent <> handlerEvents <> childEvents

    Error _err -> runEvent fromEvent (cont state)

  Html kind children -> do
    childEvents' <- mapM (runEvent fromEvent) children
    pure $ concat childEvents'

  Attribute n cont -> runEvent fromEvent cont

  Hide x -> runEvent fromEvent x

  x -> pure []

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}

prepareGraph :: Purview a m -> (Purview a m, [FromEvent])
prepareGraph = prepareGraph' [] []

type Location = [Int]

prepareGraph' :: Location -> Location -> Purview a m -> (Purview a m, [FromEvent])
prepareGraph' parentLocation location component = case component of
  Attribute attrs cont ->
    let result = prepareGraph' parentLocation location cont
    in (Attribute attrs (fst result), snd result)

  Html kind children ->
    let result = fmap (\(index, child) -> prepareGraph' parentLocation (index:location) child) (zip [0..] children)
    in (Html kind (fmap fst result), concatMap snd result)

  EffectHandler _ploc _loc state handler cont ->
    let
      rest = fmap (prepareGraph' location (0:location)) cont
    in
      ( EffectHandler (Just parentLocation) (Just location) state handler (\state' -> fst (rest state'))
      , snd (rest state)
      )

  Once effect hasRun cont ->
    let send message =
          FromEvent
            { event = "once"
            , message = toJSON message
            , location = Just location
            }
    in if not hasRun then
        let
          rest = prepareGraph' parentLocation location cont
        in
          (Once effect True (fst rest), [effect send] <> (snd rest))
       else
        let
          rest = prepareGraph' parentLocation location cont
        in
          (Once effect True (fst rest), snd rest)

  Hide x ->
    let (child, actions) = prepareGraph' parentLocation location x
    in (Hide child, actions)

  component' -> (component', [])
