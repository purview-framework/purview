{-# LANGUAGE NamedFieldPuns #-}
-- | AAAAAAAAAAAA

module Experiment where

-- | yes

data Component a b = Component
  { state   :: a
  , handler :: a -> b -> a
  , render  :: a -> String
  }

-- | we want handlers with limited scope, or do we?

data Todo = Todo
  { done :: Bool
  , task :: String
  }

data TodoState = TodoState
  { name :: String
  , todos :: [Todo]
  }

todoState = TodoState
  { name = ""
  , todos = []
  }

data TodoItemActions
  = Done
  | UnDone

mkTodoItem state = Component
  { state = state :: Todo
  , handler = \state message -> case message of
      Done -> state
      UnDone -> state
  , render = \state -> ""
  }

render' TodoState{ name, todos } =
  let todos' = fmap mkTodoItem todos
      rendered = foldr (\t acc -> render t (state t) <> acc) "" todos'
  in
    name
    <> rendered

data TodoActions =
  SetName

todoComponent = Component
  { state   = todoState
  , handler = \state message -> case message of
      SetName -> state
  , render  = render'
  }
