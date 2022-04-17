{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Typings where

import           Prelude hiding (div)
import           Data.Aeson
import           Data.Aeson.TH

import Purview

-- helpers
-- input = Html "input"
-- button = Html "button"
-- ul = Html "ul"
-- li = Html "li"
--
-- nameAttr = Attribute . Generic "name"
-- typeAttr = Attribute . Generic "type"
-- checkedAttr = Attribute . Generic "checked"
--
-- data Fields = Fields { description :: String }
-- data Actions = Submit Fields | Toggle Int
--
-- data Todo = Todo { description :: String, done :: Bool }
--   deriving (Eq)
--
-- $(deriveJSON defaultOptions  ''Fields)
-- $(deriveJSON defaultOptions  ''Actions)
-- $(deriveJSON defaultOptions  ''Todo)
--
-- handler :: ([Todo] -> Purview Actions String IO) -> Purview String Actions IO
-- handler = effectHandler ([] :: [Todo]) action
--   where
--     -- hmm, a little ungainly having to specify
--     -- action :: Actions -> [Todo] -> IO ([Todo], [DirectedEvent () Actions])
--
--     action (Submit Fields { description }) todos = pure $
--       (todos <> [Todo { description=description, done=False }], [])
--
--     action (Toggle n) todos =
--       let change (index, todo@Todo { done=alreadyDone }) =
--             if index == n
--             then todo { done=not alreadyDone }
--             else todo
--       in pure (fmap change (zip [0..] todos), [])
--
-- topStyle = style "font-family: sans-serif"
--
-- todoItem (index, Todo { description, done }) = div
--   [ text description
--   , onClick (Toggle index)
--       $ typeAttr "checkbox"
--       $ (if done then checkedAttr "checked" else id)
--       $ input []
--   ]
--
-- -- overall view
-- container = style "font-size: 24px" . div
--
-- view todos = container
--   [ div $ fmap todoItem (zip [0..] todos)
--   , formHandler $ const addNewTodoForm
--   ]
--
-- -- submission form
-- submitButton = typeAttr "submit" $ button [ text "submit" ]
--
-- defaultFields = Fields { description="" }
--
-- formHandler = effectHandler ([] :: [String]) action
--   where
--     action newTodo state = pure (state, [Parent (Submit newTodo)])
--
-- addNewTodoForm =
--   div
--     [ onSubmit defaultFields $
--         form
--           [ nameAttr "description" $ input []
--           , submitButton
--           ]
--     ]
--
-- top :: (String -> Purview String String IO) -> Purview String String IO
-- top = effectHandler "" reducer
--   where
--     -- reducer :: Monad m => String -> String -> m (String, [DirectedEvent () String])
--     reducer _ _ = pure ("", [])
--
-- main = Purview.run (defaultConfiguration { component=top $ const $ handler view })

other =
  let reducer action state = pure $ ("", [])
  in effectHandler ("" :: String) reducer (const (text ""))

-- top = effectHandler ("" :: String) reducer (const (text ""))
--   where
--     reducer action state = pure $ ("", [])

-- main = Purview.run (defaultConfiguration { component=handler view })

main = Purview.run (defaultConfiguration { component=other })
