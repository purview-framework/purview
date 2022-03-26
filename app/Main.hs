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
data NewTodo = NewTodo { description :: String }
data Actions = Submit NewTodo | Wumbo String

$(deriveJSON defaultOptions  ''NewTodo)
$(deriveJSON defaultOptions  ''Actions)

handler = effectHandler [] action
  where
    -- hmm, a little ungainly having to specify
    action :: Actions -> [String] -> IO ([String], [DirectedEvent Actions Actions])
    action (Submit NewTodo { description }) todos = pure $ (todos <> [description], [])

-- overall view
view todos = div
  [ ul $ fmap (\todo -> li [ text todo ]) todos
  , addNewTodoForm
  ]

-- submission form
submitButton = typeAttr "submit" $ button [ text "submit" ]

defaultNewTodo = NewTodo { description="" }

formHandler = effectHandler ([] :: [String]) action
  where
    action newTodo state = pure $ (state, [Parent (Submit newTodo)])

addNewTodoForm =
  div
    [ onSubmit defaultNewTodo $
        form
          [ nameAttr "description" $ input []
          , submitButton
          ]
    ]

main = run print (handler (\todos -> (formHandler (\rejected -> view todos))))

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
