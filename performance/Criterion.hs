{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Criterion where

import Prelude hiding (div)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)

import Criterion.Main hiding (component, input)

import Purview

{-

Using the todo example since it's big

-}

-- input = Html "input"
ul = Html "ul"
li = Html "li"

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"
checkedAttr = Attribute . Generic "checked"

data Fields = Fields { description :: ByteString }
  deriving (Show, Eq)
data Actions = Submit Fields | Toggle Int
  deriving (Show, Eq)

data Todo = Todo { description :: ByteString, done :: Bool }
  deriving (Show, Eq)

handler'' = effectHandler [] [] action
  where
    -- hmm, a little ungainly having to specify
    -- action :: Actions -> [Todo] -> IO ([Todo], [DirectedEvent Actions Actions])

    action (Submit Fields { description }) todos = pure $
      (const $ todos <> [Todo { description=description, done=False }], [])

    action (Toggle n) todos =
      let change (index, todo@Todo { done=alreadyDone }) =
            if index == n
            then todo { done=not alreadyDone }
            else todo
      in pure (const $ fmap change (zip [0..] todos), [])

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

formHandler = effectHandler [] ([] :: [String]) action
  where
    action newTodo state = pure (const state, [Parent (Submit newTodo)])

toForm Nothing  = Fields ""
toForm (Just _) = Fields ""

addNewTodoForm =
  div
    [ onSubmit toForm $
        form
          [ nameAttr "description" $ input []
          , submitButton
          ]
    ]

component' :: Purview () IO
component' = handler'' view

-- clickEvent = FromFrontendEvent { event="click", message="up", location=Nothing }
--
-- newStateEvent = FromFrontendEvent { event="newState", message="up", location=Nothing }

main = defaultMain
  [ bgroup "render"
        [ bench "todo example"  $ whnf render component' ]
--  , bgroup "prepareTree"
--        [ bench "todo example" $ whnf prepareTree component' ]
--  , bgroup "apply event"
--        [ bench "todo example" $ whnf (runEvent clickEvent) component' ]
--  , bgroup "new state event"
--        [ bench "todo example" $ whnf (applyNewState clickEvent) component' ]
  ]
