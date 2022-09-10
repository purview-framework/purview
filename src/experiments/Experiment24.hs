{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Experiment24 where

import Data.Proxy
import Data.Type.Equality
import Data.Typeable
import GHC.TypeLits
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.Kind

-- back to actually learning things

{-

Trying and failing again (for fun!)

-}

data Tree parentEvent where
  Node ::
    { allowedEvents :: Proxy event
    , getEvents :: nodeEvent -> [Either parentEvent nodeEvent]
    , children :: [Tree nodeEvent]
    } -> Tree parentEvent

-- showEvents :: ToJSON event => event -> String
-- showEvents = show . encode

data Container where
  Container :: ToJSON event => Maybe event -> Container

defaultEvent = (Nothing :: Maybe ())

data JsonAble where
  Not :: JsonAble
  Is  :: FromJSON a => a -> JsonAble

-- right, only interested in parent
-- setType :: Either a b -> JsonAble
-- setType (Right _) = Not
-- setType (Left x)  = Is x

userEvent = Right ""

-- take a default maybe and a new maybe.
-- if the new maybe is Nothing, return the default
-- mkEvent :: forall a b c. Maybe a -> Maybe b -> Maybe c
-- mkEvent defaultEvent Nothing = defaultEvent
-- mkEvent _            event   = event

-- so I need some a -> c & b -> c

-- transfer :: forall a b. Maybe a -> Maybe b
-- transfer (Just x) =

{-

Trying and failing to default types again below

-}

-- funcy :: forall a. Typeable a => Maybe a -> Bool
-- funcy a = case eqT @a @() of
--   Just Refl -> True
--   _         -> False
--
--
-- zup = funcy Nothing


-- what do I want?
-- default types

-- data F a where
--   F :: { parent :: Proxy a, child :: Proxy b } -> F a
--
-- default' = F (Proxy @()) (Proxy @())
--
-- glug :: F a -> Proxy a
-- glug (F p _) = p
--
-- mkF :: forall a b c d. Either a b -> F c -> F (Maybe c)
-- mkF ins x = F (Proxy @(Maybe c)) (Proxy @b)
-- --
-- goa = mkF (Right "") default'
--
-- -- maybe I should just think harder hurr
-- data Tracks a m where
--   Container ::
--     { action :: action
--     , monad  :: monad
--     , continue :: [Tracks action monad]
--     } -> Tracks a m
--
-- -- one: why doesn't rankN fromJSON work?
-- data Smaller a where
--   Sep ::
--     (Show parentEvent, ToJSON parentEvent)
--     => parentEvent
--     -> Smaller a
--
-- -- data Lie = Lie
-- --
-- -- test = Sep Lie
--
-- needs :: (Show a, ToJSON a) => a -> String
-- needs = show . encode
--
-- sup = needs 0
-- sup2 = needs (Right "")
--
-- thumb (Sep x) = needs x
--
-- sucks = thumb (Sep (Right ""))

-- aah right we did get this working
-- now the problem is the applicative can't be solved

-- sink = needs Nothing

-- data Parent a = Parent a
--
-- instance Monoid (Parent a) where
--   mempty = Parent ()

-- class Heck a where
  -- get :: a -> Maybe a
  -- default get :: a -> Maybe a
  -- get _ = Nothing
--
-- instance (ToJSON a) => Heck a where
  -- get x = Just x
--
-- z = get $ Nothing
-- y = Just "sup"

-- two: can I have a default?

-- an array of either
-- test = [Right "hello"]
--
-- -- has an undefined a
-- thimble :: forall a b. Either a b -> Either (Proxy ()) (Proxy b)
-- thimble _ = undefined
--
-- check :: forall a b. Either a b -> Proxy a
-- check _ = Proxy @a
--
-- hmm :: forall a b. (Typeable a, Typeable b) => Either a b -> Maybe ()
-- hmm a = case (cast a :: Maybe (Either () b)) of
--   Nothing -> Just ()
--   Just _  -> Nothing
--
-- ehh :: Proxy a -> Proxy b -> Bool
-- ehh = undefined
--
-- -- huh = hmm (Right "")
--
-- data Parent a = Parent a
--   deriving Show
--
-- instance Num () where
--
-- instance Num (Either a b) where
--
-- data Wrapper a b = PParent (Parent a) | Self b
--   deriving Show
--
-- instance Num (Wrapper a b) where
--
-- hmm2 :: forall a b. (Typeable a, Typeable b) => Wrapper a b -> Wrapper a b
-- hmm2 = id
--
-- -- hmm3 = hmm2 $ Self ""
--
-- -- flop = hmm (Left "")
--
--
-- aaa :: (a :~: ()) ->  Bool
-- aaa Refl = True


data User = User { userAdminToken :: Maybe (Proxy 'Admin) }

data UserType
  = Admin

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = undefined

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  y = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- cmpSymbol :: forall a b proxy1 proxy2. (KnownSymbol a, KnownSymbol b)
--           => proxy1 a -> proxy2 b -> OrderingI a b
-- cmpSymbol x y = case compare (symbolVal x) (symbolVal y) of
--   EQ -> case unsafeCoerce (Refl, Refl) :: (CmpSymbol a b :~: 'EQ, a :~: b) of
--     (Refl, Refl) -> EQI
--   LT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'LT) of
--     Refl -> LTI
--   GT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'GT) of
--     Refl -> GTI

lame = Nothing
lux  = Nothing :: Maybe ()
flunk = Nothing :: Maybe String

getta :: forall a b. Maybe (a :~: b) -> Maybe a -> Maybe b -> [Maybe a]
getta Nothing _ _ = []
getta (Just Refl) x y = [x, y]

ambiguous   = Nothing
unambiguous = Nothing :: Maybe String

locka :: forall a. Maybe (a :~: ()) -> Maybe a -> Maybe a
locka Nothing x     = x
locka (Just Refl) x = x

data DirectionAction where
  Parent :: action -> DirectionAction
  Self :: action -> DirectionAction

-- zing = locka (Just Refl) unambiguous

y = Proxy @(Maybe String)
x = Proxy @(Maybe ())

type family Has (a :: Bool) (b :: Type) :: Type where
  Has 'True  b = ()
  Has 'False b = b

checka :: Maybe a -> Maybe (Has (a == ()) a)
checka Nothing  = Nothing
checka (Just x) = undefined

hella :: ToJSON a => a -> String
hella = show . encode

-- so we want the top level to replace the children?

-- fk = hella lame

-- type family Zogg (a :: Type) (b :: Type) :: Type where
--   Zogg a b = a
--   Zogg a a = a
--
-- lug :: forall a b. Maybe a -> Maybe b -> Maybe (Zogg a b)
-- lug = undefined

-- lug :: forall a. Maybe a -> Maybe (Zogg a)
-- lug = id

-- so with this we can shove over a value...

-- f = encode (locka (Just Refl) unambiguous)

-- and it works!

-- zimple :: Maybe a -> Maybe b -> [Maybe a]
-- zimple x y = [x, y]

-- so it can at least move the Maybe a to a Maybe ()

z = Proxy :: Proxy String

-- a function that creates a value that has a fromJSON ?


data Flumox a m where
  Flumox :: (ToJSON a, ToJSON b, Applicative m) => { able :: [Either a b], interp :: Proxy m } -> Flumox a m

defaultFlumox = Flumox ([] :: [Either () ()]) (Proxy @IO)

-- instance (a ~ ()) => ToJSON (Either a b) where

-- so......

data WithDefault k (a :: k) where
  Default :: WithDefault k a
  Value   :: k -> WithDefault k a

-- exa = Flumox (Right "")
--
-- defaultFlumox = undefined
--
-- printa Flumox { able } = encode able

ejemplo :: Applicative m => m String
ejemplo = pure "hello"

okay = ejemplo
