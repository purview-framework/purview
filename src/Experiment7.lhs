 {-# LANGUAGE OverloadedStrings #-}

> {-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE TypeApplications #-}

> {-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DataKinds #-}

> {-# LANGUAGE GADTs #-}

 {-# LANGUAGE TypeOperators #-}
 {-# LANGUAGE LambdaCase #-}

> {-# LANGUAGE FlexibleContexts #-}

> module Experiment7 where
>
> import Control.Monad.Freer (Eff, Member, send)

Can we just use algebraic effects to represent hooks?

Let's start with a goal of what we want to be able to write, loosely:

getPosts postId setPosts = do
  raw <- get https://jsonplaceholder.typicode.com/posts/<> postId <> /comments
  setPosts json raw

commentsView postId = do
  (setPosts, posts) <- useState([])

  useEffect(getPosts postId setPosts, [postId])

  fmap show posts

commentsComponent = do
  (setPostId, postId) <- useState(1)

  comments <- commentsView postId

  [ onClick (setPostId (postId + 1))
  , comments
  ]

things to note:
- state should persist
- the effect should only happen if the dependecies change, but at least once
- the effect is non-blocking
- onClick means some external input

each of these are pretty interesting in their own way, gonna be tricky

let's take a rough stab at useState

First we define the effect

 data State s r where
   UseState :: State s s

Breaking it down, it's like:
UseStore :: type of initial state
         -> returns a store
         -> and unwraps to a tuple of a function a -> a (setState)
         -> and a (the state, or really getState)

now for the interface

 useState :: forall s effs. Member (State s) effs => Eff effs s
 useState = send (UseState)

 useState :: Has State sig m => a -> m (a -> m (), m a)
 useState initialState = send (UseState initialState)

let's see how it looks

> data State s r where
>   Get :: State s s
>   Put :: s -> State s ()

> data Stateful s r where
>   UseState :: s -> Stateful s (s -> State s (), State s s)

> putState :: forall s effs. Member (State s) effs => s -> Eff effs ()
> putState x = send (Put x)

> getState :: forall s effs. Member (State s) effs => Eff effs s
> getState = send Get
>
> useState :: forall s effs.
>       ( Member (Stateful s) effs
>       , Member (State s) effs
>       ) => s -> (s -> Eff effs (), Eff effs s)
> useState initialState = do
>   (\s -> send (Put s), send Get)

> stateTest ::
>       ( Member (State String) effs
>       , Member (Stateful String) effs)
>       => Eff effs String
> stateTest = do
>   let (setName, getName) = useState ""
>   setName "waluigi"
>   getName

   state <- getState
   let y = state <> "hallo"
   pure y

that typechecks, so now we need a way to interpret the effect

 data State s r where
   Get :: State s s
   Put :: !s -> State s ()

 -- | Retrieve the current value of the state of type @s :: *@.
 get :: forall s effs. Member (State s) effs => Eff effs s
 get = send Get

 -- | Set the current state to a specified value of type @s :: *@.
 put :: forall s effs. Member (State s) effs => s -> Eff effs ()
 put s = send (Put s)

 stateTest :: Member (State String) effs => Eff effs String
 stateTest = do
    put "sup"
    (<> "hallo") <$> get
