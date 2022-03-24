{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude      hiding (div)
import           GHC.Generics
import           Purview

-- for server time example
import           Data.Time
import           Data.Aeson
import           Data.Aeson.TH

import Control.Monad.Writer


-----------------------
-- Todo List Example --
-----------------------

-- helpers
input = Html "input"
button = Html "button"
ul = Html "ul"
li = Html "li"

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"

-- actions
newtype NewTodo = NewTodo { description :: String }
data Actions = Submit NewTodo | Wumbo String

$(deriveJSON defaultOptions  ''NewTodo)
$(deriveJSON defaultOptions  ''Actions)

-- handler
handler = messageHandler [] action
  where
    action (Submit NewTodo { description }) todos = todos <> [description]

-- overall view
view todos = div
  [ ul $ fmap (\todo -> li [ text todo ]) todos
  , addNewTodoForm
  ]

-- submission form
submitButton = typeAttr "submit" $ button [ text "submit" ]

defaultNewTodo = NewTodo { description="" }

test send = messageHandler ([] :: [String]) action $ \state -> (text "")
  where
    action (NewTodo { description }) state = do
      tell $ send (Submit (NewTodo description))
      pure []

-- x = Jack test

{-

Hmm true, Submit NewTodo _is not_ the default values for the form

NewTodo alone is

So there needs to be a way to specify doing something with the form values
and then passing it up

Maybe form should be special?  No.

There needs to be a way to pass an action up the chain, form can be a short
hand for whatever method is needed to pass an event back up

messageHandler
  join parentLocation
    passes down a fn that can be used to emit an event?
      messageHandler
        form

Alternately, add a transform fn to onClick / onSubmit that would take in the value
and produce the event?

-}

addNewTodoForm =
  div
    [ onSubmit (Submit defaultNewTodo) $
        form
          [ nameAttr "description" $ input []
          , submitButton
          ]
    ]

main = run print (handler view)

-------------------------
-- Using Input Example --
-------------------------

-- input = Html "input"
-- button = Html "button"
--
-- nameAttr = Attribute . Generic "name"
-- typeAttr = Attribute . Generic "type"
--
-- data Fields = Fields
--   { textField :: String
--   }
--
-- $(deriveJSON defaultOptions ''Fields)
--
-- handler = messageHandler "" action
--   where
--     action (Fields txt) _ = txt
--
-- submitButton = typeAttr "submit" $ button [ text "submit" ]
--
-- defaultFields = Fields { textField="" }
--
-- display txt = div
--   [ text ("you submitted: " <> txt)
--   , onSubmit defaultFields $ form
--     [ nameAttr "textField" $ input []
--     , submitButton
--     ]
--   ]
--
-- main = run print (handler display)

----------------------------
-- Expanding List Example --
----------------------------

-- data Operation = AddItem | RemoveItem
--
-- $(deriveJSON defaultOptions ''Operation)
--
-- handler = messageHandler ([] :: [String]) action
--   where
--     action AddItem state = "item" : state
--     action RemoveItem (x:xs) = xs
--     action RemoveItem [] = []
--
-- display items = div
--   [ div $ fmap (\item -> div [ text item ]) items
--   , onClick AddItem (div [ text "add" ])
--   , onClick RemoveItem (div [ text "remove" ])
--   ]
--
-- main = run print (handler display)

---------------------
-- Stepper Example --
---------------------

-- data Direction = Up | Down
--
-- $(deriveJSON defaultOptions ''Direction)
--
-- data OtherDirection = Port | Starboard
--
-- $(deriveJSON defaultOptions ''OtherDirection)
--
-- upButton = onClick Up $ div [ text "up" ]
-- downButton = onClick Down $ div [ text "down" ]
--
-- handler = messageHandler (0 :: Int) action
--   where
--     action Up   state = state + 1
--     action Down state = state - 1
--
-- handler' = messageHandler (0 :: Int) action
--   where
--     action Port      state = state + 1
--     action Starboard state = state - 1
--
-- component' = handler' (\state -> div [ text "" ])
--
-- counter :: Int -> Purview Direction
-- counter state = div
--   [ upButton
--   , text $ "count: " <> show state
--   , downButton
--   ]
--
-- component = handler counter
--
-- multiCounter = div
--   [ component
--   , component
--   , component
--   ]
--
-- logger = print
--
-- main = run logger component


-------------------------
-- Server Time Example --
-------------------------

-- data UpdateTime = UpdateTime
--
-- $(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''UpdateTime)
--
-- display :: Maybe UTCTime -> Purview a
-- display time = div
--   [ text (show time)
--   , onClick UpdateTime $ div [ text "check time" ]
--   ]
--
--startClock cont state = Once temp False (cont state)
--  where temp send = do
--          send UpdateTime
--
-- timeHandler = effectHandler Nothing handle
--   where
--     handle UpdateTime state = Just <$> getCurrentTime

-- main = run logger (timeHandler (startClock display))
