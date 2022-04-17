{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Component where

import           Data.Aeson
import           Data.Typeable

import           Events

data Attributes action where
  On :: ToJSON action => String -> action -> Attributes action
  Style :: String -> Attributes action
  Generic :: String -> String -> Attributes action

type Identifier = Maybe [Int]
type ParentIdentifier = Identifier

data Purview parentAction action m where
  Attribute :: Attributes action -> Purview parentAction action m -> Purview parentAction action m
  Text :: String -> Purview parentAction action m
  Html :: String -> [Purview parentAction action m] -> Purview parentAction action m
  Value :: Show a => a -> Purview parentAction action m

  EffectHandler
    :: ( FromJSON newAction
       , ToJSON newAction
       , ToJSON parentAction
       , FromJSON state
       , ToJSON state
       , Typeable state
       , Eq state
       )
    => ParentIdentifier
    -> Identifier
    -> state
    -> (newAction-> state -> m (state, [DirectedEvent parentAction newAction]))
    -> (state -> Purview newAction any m)
    -> Purview parentAction newAction m

  Once
    :: (ToJSON action)
    => ((action -> FromEvent) -> FromEvent)
    -> Bool  -- has run
    -> Purview parentAction action m
    -> Purview parentAction action m

  Hide :: Purview parentAction newAction m -> Purview parentAction any m

instance Show (Purview parentAction action m) where
  show (EffectHandler parentLocation location state _action cont) =
    "EffectHandler " <> show parentLocation <> " " <> show location <> " " <> show (cont state)
  show (Once _ hasRun cont) = "Once " <> show hasRun <> " " <> show cont
  show (Attribute _attrs cont) = "Attr " <> show cont
  show (Text str) = show str
  show (Html kind children) =
    kind <> " [ " <> concatMap ((<>) " " . show) children <> " ] "
  show (Value value) = show value
  show (Hide a) = "Hide " <> show a

instance Eq (Purview parentAction action m) where
  a == b = show a == show b

-- Various helpers
div :: [Purview parentAction action m] -> Purview parentAction action m
div = Html "div"

form :: [Purview parentAction action m] -> Purview parentAction action m
form = Html "form"

text :: String -> Purview parentAction action m
text = Text

style :: String -> Purview parentAction action m -> Purview parentAction action m
style = Attribute . Style

onClick :: ToJSON action => action -> Purview parentAction action m -> Purview parentAction action m
onClick = Attribute . On "click"

onSubmit :: ToJSON action => action -> Purview parentAction action m -> Purview parentAction action m
onSubmit = Attribute . On "submit"

identifier :: String -> Purview parentAction action m -> Purview parentAction action m
identifier = Attribute . Generic "id"

classes :: [String] -> Purview parentAction action m -> Purview parentAction action m
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

once sendAction = Once sendAction False
