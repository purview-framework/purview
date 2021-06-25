> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}
> module Experiment3 where
>
> import Prelude hiding (div)

looks like the current css quasiquoter doesn't support merging

style = "width: 50px;"

todo Todo{text, id} =
  [div [onClick (MarkDone id)] [text text]]

style todo

todoList TodoList{name, todos} =
  [ div [] [ text name
           , map todo todos
           ]
  ]

todoComponent

events flow up to a component?  that seems reasonable
global for messages that should go... anywhere?

subscribing to the state is the same thing as graphql, so maybe
we haven't really moved away from it

elm doesn't seem extendable

I have to give it _something_ to trigger a render.  I think?

todo (id, text', checked) =
  div
    [onClick (MarkDone id)]
    [text text']

todoList :: String -> [(String, String, Bool)] -> undefined
todoList name todos =
  div []
    [ text name
    , fmap todo todos
    ]

getTodos =
  if not loading local then
    forkIO $
      local "todos loaded"
      global "absolutely done"
    []
  else
    todos local

todoComponent = do
  todos <- getTodos
  todoList "my list" todos

Could I build with with an effect?

So we want handlers to be contained... we also want to allow
cross system messages.

The problem is that you can either have private state or shared state
, never both, and even worse, the moment you need to share state
between widgets you must lose the private state of and TEA-fy
the whole widget hierarchy beneath the state-sharing widget.

Component
  Style ""
  OnClick ""
  Handles OnClick
  Html ""

> data Test = Test { name :: String, other :: String }

Nope, that won't work

The problem is that you can either have private state or shared state, never both, and
even worse, the moment you need to share state between widgets you must lose the private
state of and TEA-fy the whole widget hierarchy beneath the state-sharing widget.

For example:

list :: [String] -> Widget a
list items = do
  newItem <- div [] $ mconcat
    [ [ renderItem item | item <- items ]
    , [ inputOnEnter ]
    ]
  list (newItem:items)

Of the many so-so solutions, the one that almost entirely solves the problem is
automatically persisted component local state. If I port concur to react-basic-hooks,
It would be possible to have a function like useState which allows access to the
previous state of the component, without having the state be explicitely passed in.
I previously got this same architecture working in the halogen-vdom backend as well,
however that code was not made public and I lost the working version of it. I will
need to restore that.


automatically persisted component local state

> data F t a = F [ F a t ]

> data Widget t = Widget { state :: Maybe t, html :: Maybe t -> [ Renderable ] }
>               | S String

> todoList = Widget Nothing (\s ->
>   [ pack $ S "todo list", todoItem, todoEntry ])

> todoItem = pack $ Widget (Just True) (\checked -> [])

> todoEntry = pack $ Widget (Just "") (\_ -> [])

> class Render a where
>   renderIt :: a -> String
>
> instance Render (Widget t) where
>   renderIt w = "" --(html w) (state w)
>
> data Renderable = forall a . Render a => MkRenderable a
>
> pack :: Render a => a -> Renderable
> pack = MkRenderable
>
>
> text = pack . S
> div _attr = pack . Widget Nothing . const
>
> todoItem' = div [] [ text "" ]
>
>
