-- |

> {-# LANGUAGE NamedFieldPuns, TupleSections, TypeApplications, OverloadedStrings, DeriveFunctor, KindSignatures, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

> module Experiment7 where
>
> import Control.Algebra (Has, send)
> import Data.Kind (Type)

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

> data State (m :: Type -> Type) k where
>   UseState :: a -> State m (a -> m (), m a)

Breaking it down, it's like:
UseStore :: type of initial state
         -> returns a store
         -> and unwraps to a tuple of a function a -> a (setState)
         -> and a (the state, or really getState)

now for the interface

> useState :: Has State sig m => a -> m (a -> m (), m a)
> useState initialState = send (UseState initialState)

let's see how it looks

> stateTest :: (Has State sig m) => m Int
> stateTest = do
>   (setState, s) <- useState 0
>   setState 2
>   s

that typechecks, so now we need a way to interpret the effect
