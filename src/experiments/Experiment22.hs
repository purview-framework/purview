{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Experiment22 where


type Hmm a b = Tree a b

data Tree parentAction action where
  Sender
    :: action
    -> [Tree parentAction action]
    -> Tree parentAction action
  Reducer
    :: forall action newAction. (newAction -> [Either action newAction])
    -> [Tree action newAction]
    -> Hmm action newAction

data Lower = North | South
data Upper = East | West

type S action = forall parentAction . Tree parentAction action

childSender :: S Lower
childSender = Sender North []

child = Reducer handler [ childSender ]
  where handler a = [Left East]

parentSender = Sender East []

-- parent :: Tree ()
-- parent = Reducer handler [ child, parentSender ]
--   where handler :: Upper -> [Either () Upper]
--         handler = undefined

-- data Reducer parentAction action where
--   Reducer
--     :: (action -> [Either parentAction action])
--     -> [Tree action]
--     -> Reducer parentAction action
--
-- data Sender action where
--   Sender
--     :: action
--     -> [Tree action]
--     -> Sender action
--

-- data Tree action
--   = Sender action [Tree action]
--   | newAction . Reducer (newAction -> [Either action newAction]) [Tree newAction]

-- data Lower = North | South
-- data Upper = East | West
--
-- childSender = Sender' (Sender North [])
--
-- child = Reducer' $ Reducer handler [ childSender ]
--   where handler a = [Left East]
--
-- parentSender = Sender' $ Sender East []
--
-- parent = Reducer' $ Reducer handler [ child, parentSender ]
--   where handler :: Upper -> [Either () Upper]
--         handler = undefined


--  Reducer
--    :: (parentAction -> action -> [Either parentAction action])
--    -> [Tree action]
--    -> Tree action
