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

data Purview a where
  Attribute :: Attributes a -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: Show a => a -> Purview a

  EffectHandler
    :: (FromJSON action, FromJSON state, ToJSON action, ToJSON a, ToJSON state, Typeable state, Eq state)
    => ParentIdentifier
    -> Identifier
    -> state
    -> (action -> state -> IO (state, [DirectedEvent a action]))
    -> (state -> Purview action)
    -> Purview action

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview a
    -> Purview a

  Hide :: Purview a -> Purview b

instance Show (Purview a) where
  show (EffectHandler parentLocation location state _action cont) =
    "EffectHandler " <> show parentLocation <> " " <> show location <> " " <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute _attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value
  show (Hide a) = "Hide " <> show a

instance Eq (Purview a) where
  a == b = show a == show b

-- Various helpers
div :: [Purview a] -> Purview a
div = Html "div"

form :: [Purview a] -> Purview a
form = Html "form"

text :: String -> Purview a
text = Text

style :: String -> Purview a -> Purview a
style = Attribute . Style

onClick :: ToJSON b => b -> Purview b -> Purview b
onClick = Attribute . OnClick

onSubmit :: ToJSON b => b -> Purview b -> Purview b
onSubmit = Attribute . OnSubmit

identifier :: String -> Purview a -> Purview a
identifier = Attribute . Generic "id"

classes :: [String] -> Purview a -> Purview a
classes xs = Attribute . Generic "class" $ unwords xs

effectHandler
  :: (FromJSON action, FromJSON state, ToJSON action, ToJSON parent, ToJSON state, Typeable state, Eq state)
  => state
  -> (action -> state -> IO (state, [DirectedEvent parent action]))
  -> (state -> Purview action)
  -> Purview a
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

render :: Purview a -> String
render = render' []

render' :: [Attributes a] -> Purview a -> String
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

applyNewState :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
applyNewState eventBus fromEvent@FromEvent { message, location } component = case component of
  EffectHandler ploc loc state handler cont -> case fromJSON message of
    Success newState -> do
      if loc == location
        then pure $ EffectHandler ploc loc newState handler cont
        -- TODO: continue down the tree
        else pure $ EffectHandler ploc loc state handler cont
    Error _ -> do
      pure $ EffectHandler ploc loc state handler cont

  Hide x -> do
    children <- applyNewState eventBus fromEvent x
    pure $ Hide children

  -- TODO: continue down the tree
  x -> pure x

applyEvent :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
applyEvent eventBus fromEvent@FromEvent { message, location } component = case component of
  EffectHandler parentLocation loc state handler cont -> case fromJSON message of
    Success parsedAction -> do
      void . forkIO $ do
        -- if locations match, we actually run what is in the handler
        (newState, events) <-
          if loc == location
          then handler parsedAction state
          else pure (state, [])

        -- although it doesn't break anything, only send this when the
        -- locations match (cuts down on noise)
        when (loc == location) $ atomically $ writeTChan eventBus $ FromEvent
          { event = "newState"
          , message = toJSON newState
          , location = loc
          }

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
        mapM_ (atomically . writeTChan eventBus . createMessage) events

      -- ok, right, no where in this function does the tree actually change
      -- that's handled by the setting state event
      _ <- applyEvent eventBus fromEvent (cont state)

      -- so we can ignore the results from applyEvent and continue
      pure $ EffectHandler parentLocation loc state handler cont

    Error _err -> do
      _ <- applyEvent eventBus fromEvent (cont state)
      pure $ EffectHandler parentLocation loc state handler cont

  Html kind children -> do
    children' <- mapM (applyEvent eventBus fromEvent) children
    pure $ Html kind children'

  Attribute n cont -> do
    child <- applyEvent eventBus fromEvent cont
    pure $ Attribute n child

  Hide x -> do
    child <- applyEvent eventBus fromEvent x
    pure $ Hide child

  x -> pure x

{-|

For now the only special event kind is "newState" which replaces
the inner state of a Handler (Message or Effect)

-}

apply :: TChan FromEvent -> FromEvent -> Purview a -> IO (Purview a)
apply eventBus fromEvent@FromEvent {event=eventKind} component =
  case eventKind of
    "newState" -> applyNewState eventBus fromEvent component
    _          -> applyEvent eventBus fromEvent component

{-|

This walks through the tree and collects actions that should be run
only once, and sets their run value to True.  It's up to something
else to actually send the actions.

It also assigns a location to message and effect handlers.

-}

prepareGraph :: Purview a -> (Purview a, [FromEvent])
prepareGraph = prepareGraph' [] []

type Location = [Int]

prepareGraph' :: Location -> Location -> Purview a -> (Purview a, [FromEvent])
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
