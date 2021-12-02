{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
-- |

module Experiment13 where

import Prelude hiding (div)

import Data.Typeable
import GHC.Generics
import Control.Concurrent.STM.TChan

{-

Maybe I should try a new technique that isn't based entirely
around a record

-}

-- data Purview a where
--   Text :: String -> Purview a
--   Html :: String -> Purview a -> Purview a
--   -- UseState :: state -> ((state, state -> ()) -> Purview a) -> Purview a
--   -- Connect ::
--   Handler
--     :: (Typeable messages)
--     => state
--     -> (messages -> state)
--     -> (state -> Purview messages)
--     -> Purview messages
--
-- t = Html "a"
--
-- x state = Html (show state) (Text (show state))
--
-- data Actions = Up | Down
--   deriving Show
--
-- y = Handler Up handle x
--   where
--     handle Up = Down
--     handle Down = Up
--
-- walk :: Actions -> Purview a -> String
-- walk action (Handler st handle rest) =
--   let newSt = maybe st handle (cast action)
--   in walk action (rest newSt)
-- walk _ (Html kind children) = kind

{-

Next thing to figure out is actions

State -> passes down the state and a way to set the state
OnChange -> runs when params passed in change, but once by default

-}

-- newtype Attribute a = OnClick a
--
-- data Purview a where
--   Text :: String -> Purview a
--   Html :: String -> [Attribute a] -> Purview a -> Purview a
--   State :: state -> ((state, state -> ()) -> Purview a) -> Purview a
--   -- Handler
--   -- OnChange
--   -- Once
--
-- data AnnPurview a where
--   AnnText :: Purview a -> AnnPurview a
--   AnnState :: String -> Purview a -> AnnPurview a
--
-- comp (state, setState) = Html "div" [OnClick (setState (state + 1))] (Text "hello")
--
-- state = State (0 :: Integer)
--
-- comb = state comp

{-

Somehow have to turn that setState into sending a message

Maybe it should just be done in terms of handler?

Imagine a handler with no state.  Maybe the message is just
analytics.

-}

-- newtype Attribute a = OnClick a
--
-- data Purview a where
--   Text :: String -> Purview a
--   Html :: String -> [Attribute a] -> Purview a -> Purview a
--   State :: state -> ((state, state -> ()) -> Purview a) -> Purview a
--   Handler
--     :: (Typeable action)
--     => (action -> ())
--     -> ((action -> ()) -> Purview a)
--     -> Purview a
--   -- Handler
--   -- OnChange
--   -- Once
--
-- data Action = Up | Down
--
-- x send = Html "div" [OnClick (send Up)] (Text "")
--
-- handle (state, setState) = Handler handler x
--   where handler Up = setState Down
--         handler Down = setState Up
--
-- comb' = State Up handle
--
-- comp (state, setState) = Html "div" [OnClick (setState (state + 1))] (Text "hello")
--
-- state = State (0 :: Integer)
--
-- comb = state comp

{-

Round 4 - rewriting so that components can tell themselves to rerender

Actually for now let's skip that.  Ok right, setState needs to trigger a
rerender somehow.  A handler calling setState.

-}


data Attributes = OnClick | Style

data Purview a where
  Attribute :: Attributes -> b -> Purview a -> Purview a
  Text :: String -> Purview a
  Html :: String -> [Purview a] -> Purview a
  Value :: a -> Purview a

  State :: state -> ((state, state -> m ()) -> Purview a) -> Purview a
  MessageHandler
    :: (Typeable action)
    => (action -> m ())
    -> ((action -> b) -> Purview a)
    -> Purview a
  Once :: (action -> ()) -> Purview a -> Purview a

-- a little bit to clean up defining these
div = Html "div"
text = Text
useState = State
onClick = Attribute OnClick

-- nothing but display
---- display count = div (text $ "You clicked: " <> show count <> " times")
--
-- -- wrap the child with a click handler, pass count down
-- clickHandler child (count, setCount) = onClick (setCount (count + 1)) (child count)
--
-- -- same as useState, sort of, passes to the next fn (state, setState)
-- clickState = useState 0
--
-- -- put it all together
-- clicker = clickState (clickHandler display)

-- now I'm redux-light
data ArrowDirection = Up | Down

arrowHandler child (direction, setDirection) = MessageHandler handle (child direction)
  where handle message = case message of
                           Up -> setDirection Down
                           Down -> setDirection Up

arrowState child = useState Up (arrowHandler child)

arrowFlipper direction send =
  onClick (send direction) (div $ [text "Flip the direction"])

-- we can define useEffect in terms of state and handler
data Task = Init | Attempted
  deriving Eq

taskState = useState Init

run fn (state, setState) child = MessageHandler handle (\send -> Once (send Init) child)
  where handle Init = do
          setState Attempted

          if state == Init then
            fn -- Only time we call the function
          else
            pure ()

        handle Attempted = pure ()

useEffect fn child = taskState (\(state, setState) -> run fn (state, setState) child)

test = useEffect (print "hello") (div $ [text "hi"])

-- and we get
-- counter =
--   let  x= 1
--   in H

counter :: (Integer, Integer -> m ()) -> Purview ()
counter (count, setCount) =
  let
    handleClick = Attribute OnClick (setCount (count + 1))
    style = Attribute Style ""
  in
    style $ handleClick $ Html "div" [(Text $ "You clicked: " <> show count)]

manyCounters =
  [ counter
  , counter
  ]

sharedState =
  let state = State 0
  in fmap state manyCounters

-- state = State (0 :: Integer)

-- comb = state comp


{-

More parts of the language:  Attribute

Global?  No I think you'd just use the same state + handler?

SharedState?

Calling state on a bunch of component means they do share the state

Or I guess it'd be shared if you only passed in the fns?

-}

{-

Testing?

-}

display count = div [(text $ "You clicked: " <> show count <> " times")]

-- wrap the child with a click handler, pass count down
clickHandler child (count, setCount) = onClick (setCount (count + 1)) (child count)

-- same as useState, sort of, passes to the next fn (state, setState)
clickState = useState 0

-- put it all together
clicker = clickState (clickHandler display)

-- ahaha.  AHAHAHAHA YES!
testClicker = clickState (clickHandler (\count -> Value count))

{-

let's try filling in setState

-}

data Action = Action

-- render :: TChan Action -> Purview a -> Purview a
-- render chan (State st fn) = undefined

render :: TChan String -> [Attributes] -> Purview a -> String
render out attrs tree = case tree of
  Html kind rest ->
    "<" <> kind <> ">"
    <> concatMap (render out attrs) rest <>
    "</" <> kind <> ">"

  Text val -> val

  Attribute attrs x rest ->
    case attrs of
      OnClick -> undefined

-- since state is anonymous, we do need a way to identify it
data EnrichedPurview a where
  Locatable :: [Int] -> Purview a -> EnrichedPurview a

{-

Goal: OnClick updates State which triggers a redraw

State produces an anonymous fn

That anonymous fn needs a name

No, OnClick needs a name.  It could change state, or log, or anything.

Then, State needs a way to say "rerender" once the state has changed

-}

start = do
  receive <- newTChan
  toBrowser <- newTChan

  -- interpret it all

  -- then send an update

  undefined
