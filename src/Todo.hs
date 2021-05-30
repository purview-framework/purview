{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Todo where

import Prelude hiding (div)
-- import Lib

-- I have to give it _something_ to trigger a render.  I think?

newtype Events = MarkDone String

data Html a = Html { unHTML :: a }

-- Html A -> (A -> Html B) -> Html B

data Component a b = Component
  { state :: a
  -- , html :: m b
  }


-- todo (id, text', checked) =
--   div
--     [onClick (MarkDone id)]
--     [text text']
--
-- todoList :: String -> [(String, String, Bool)] -> undefined
-- todoList name todos =
--   div
--     []
--     [ text name
--     , div [] $ fmap todo todos
--     ]
--
-- getTodos = undefined
--
-- todoComponent = do
--   todos <- getTodos
--   todoList "my list" todos
