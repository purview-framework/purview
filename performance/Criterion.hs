{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Criterion where

import Prelude hiding (div)
import           Data.Aeson
import           Data.Aeson.TH

import Criterion.Main hiding (component)

import Purview
import EventHandling
import PrepareTree (prepareTree)
import Control.Concurrent.STM (newTChan)

{-

Using the todo example since it's big

-}

input = Html "input"
button = Html "button"
ul = Html "ul"
li = Html "li"

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"
checkedAttr = Attribute . Generic "checked"

data Fields = Fields { description :: String }
data Actions = Submit Fields | Toggle Int

data Todo = Todo { description :: String, done :: Bool }
  deriving (Eq)

$(deriveJSON defaultOptions  ''Fields)
$(deriveJSON defaultOptions  ''Actions)
$(deriveJSON defaultOptions  ''Todo)

handler = effectHandler [] action
  where
    -- hmm, a little ungainly having to specify
    action :: Actions -> [Todo] -> IO ([Todo], [DirectedEvent Actions Actions])

    action (Submit Fields { description }) todos = pure $
      (todos <> [Todo { description=description, done=False }], [])

    action (Toggle n) todos =
      let change (index, todo@Todo { done=alreadyDone }) =
            if index == n
            then todo { done=not alreadyDone }
            else todo
      in pure (fmap change (zip [0..] todos), [])

topStyle = style "font-family: sans-serif"

todoItem (index, Todo { description, done }) = div
  [ text description
  , onClick (Toggle index)
      $ typeAttr "checkbox"
      $ (if done then checkedAttr "checked" else id)
      $ input []
  ]

-- overall view
container = style "font-size: 24px" . div

view todos = container
  [ div $ fmap todoItem (zip [0..] todos)
  , formHandler $ const addNewTodoForm
  ]

-- submission form
submitButton = typeAttr "submit" $ button [ text "submit" ]

defaultFields = Fields { description="" }

formHandler = effectHandler ([] :: [String]) action
  where
    action newTodo state = pure (state, [Parent (Submit newTodo)])

addNewTodoForm =
  div
    [ onSubmit defaultFields $
        form
          [ nameAttr "description" $ input []
          , submitButton
          ]
    ]

component' = handler view

clickEvent = FromEvent { event="click", message="up", location=Nothing }

newStateEvent = FromEvent { event="newState", message="up", location=Nothing }

main = defaultMain
  [ bgroup "render"
        [ bench "todo example"  $ whnf render component' ]
  , bgroup "prepareTree"
        [ bench "todo example" $ whnf prepareTree component' ]
  , bgroup "apply event"
        [ bench "todo example" $ whnf (runEvent clickEvent) component' ]
  , bgroup "new state event"
        [ bench "todo example" $ whnf (applyNewState clickEvent) component' ]
  ]
