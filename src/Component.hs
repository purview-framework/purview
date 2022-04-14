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
onClick = Attribute . On "click"

onSubmit :: ToJSON b => b -> Purview b m -> Purview b m
onSubmit = Attribute . On "submit"

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

once sendAction = Once sendAction False
