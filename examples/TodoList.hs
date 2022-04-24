{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module TodoList where

import Prelude hiding (div)

import Data.Aeson
import Data.Aeson.TH

import Purview

-------------
-- Helpers --
-------------

input = Html "input"
ul = Html "ul"
li = Html "li"

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"
checkedAttr = Attribute . Generic "checked"

-----------
-- Model --
-----------

data Fields = Fields { description :: String }

data Actions = AddNewTodo Fields | Toggle Int

data Todo = Todo { description :: String, done :: Bool }
  deriving (Eq)

$(deriveJSON defaultOptions  ''Fields)
$(deriveJSON defaultOptions  ''Actions)
$(deriveJSON defaultOptions  ''Todo)

-----------------------
-- Top level handler --
-----------------------

handler = effectHandler [] action
  where
    action (AddNewTodo Fields { description }) todos =
      let
        newTodos = todos <> [Todo { description=description, done=False }]
      in
        pure (const newTodos, [])

    action (Toggle n) todos =
      let
        change (index, todo@Todo { done=alreadyDone }) =
          if index == n
          then todo { done=not alreadyDone }
          else todo
      in
        pure (const $ fmap change (zip [0..] todos), [])

---------------------------
-- Form for adding items --
---------------------------

submitButton = typeAttr "submit" $ button [ text "submit" ]

defaultFields = Fields { description="" }

formHandler = effectHandler ([] :: [String]) action
  where
    action newTodo state = pure (const state, [Parent (AddNewTodo newTodo)])

addNewTodoForm =
  div
    [ onSubmit defaultFields $
        form
          [ nameAttr "description" $ input []
          , submitButton
          ]
    ]

------------------
-- Overall View --
------------------

topStyle = style "font-family: sans-serif"

todoItem (index, Todo { description, done }) = div
  [ text description
  , onClick (Toggle index)
      $ typeAttr "checkbox"
      $ (if done then checkedAttr "checked" else id)
      $ input []
  ]

container = style "font-size: 24px" . div

view todos = container
  [ div $ fmap todoItem (zip [0..] todos)
  , formHandler $ const addNewTodoForm
  ]


main = Purview.run (defaultConfiguration { component=handler view, devMode=True })
