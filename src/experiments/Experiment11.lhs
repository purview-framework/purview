> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE ExistentialQuantification #-}

> module Experiment11 where


> import Data.Typeable
> import Data.Dynamic

> data Some = forall a. Typeable a => Some a

> xs :: [Some]
> xs = [Some 'h', Some (123 :: Int)]

> main = putStrLn "Compiled Successfully."

> mkT :: (Typeable a, Typeable b)
>     => (b -> b) -> a -> a
> mkT f = case cast f of
>   Just g -> g
>   Nothing -> id

useEffect
useState

v

redux
@mapStateToProps
@connect

v

liveview

<Melvar> I’m particularly proud of my handler type: newtype Handler i =
         Handler (forall newhandlers . Event i newhandlers -> IO newhandlers)


useName = do
  name <- getName
  ["span" [] name]

useEffect(() => {
  function handleStatusChange(status) {
    setIsOnline(status.isOnline);
  }

  ChatAPI.subscribeToFriendStatus(props.friend.id, handleStatusChange);
  return () => {
    ChatAPI.unsubscribeFromFriendStatus(props.friend.id, handleStatusChange);
  };
}, [props.friend.id]); // Only re-subscribe if props.friend.id changes

would be something like: bracket subscribe unscribe (widget subscription)

ok, collect into tree
tree has events.  tree good.


 data Component state messages where
  Component :: (Typeable state, Typeable messages) =>
     state -> (messages -> state) -> Component state messages

> data Component state messages = Component
>  { state    :: Typeable state => state
>  , handlers :: Typeable messages => messages -> state
>  }

> data Tree
>  = Leaf String Dynamic | Node [Tree]
>
> data Actions = Up | Down
>
> comp = Component
>  0
>  (\message -> case message of
>     Up -> 1 :: Integer
>     Down -> 0
>  )
>

> tree = Node [Leaf "a" (toDyn comp)]
>

so the message we receive has an ID

> tog = case tree of
>  Node (x:xs) -> case x of
>     (Leaf "a" y) -> case fromDynamic y of
>        (Just (Component st h)) -> Leaf "a" $ toDyn (Component st h)

 case (cast h, cast st) of
           (Just h', Just st') -> Leaf "a" $ toDyn $ Component (h' Up) st' -- Component (handlers Up) handlers

fef = case tree of
 Node ((Leaf str f):xs) -> f "a"

> data HList :: [*] -> * where
>   HNil  :: HList '[]
>   HCons :: x -> HList xs -> HList (x ': xs)

> infixr 5 %:
> (%:) :: x -> HList xs -> HList (x ': xs)
> (%:) = HCons

> data ActionA = ActionA (String -> String)
> data ActionB = ActionB
> data Handler a = Handler a
>
> data CompA
>  = Increment Int
>  | Decrement Int
>
> data CompB
>  = Blek
>  | Flek
>
> y = (ActionA id) %: ActionB %: HNil
> z = ActionB %: y %: HNil
>
> zog = case y of
>  HCons (ActionA f) n -> f "hey"
