{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Experiment23 where

import           GHC.Generics
import Data.Aeson
import Data.Either
import Data.Kind
import Data.Proxy
import Data.Typeable
-- import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson

data Container = Container { name :: String } deriving (Show, Generic)

instance ToJSON Container where

containers = [Container "a", Container "b"]

one :: [Container] -> [ByteString]
one = fmap encode

two :: [Container] -> ByteString
two = encode

-- data Example where
--   Example1 :: { list1 :: ToJSON a => a } -> Example
--   Example2 :: ToJSON a => { list2 :: a } -> Example
--
-- -- test :: Example -> Example
-- -- test (Example1 a) = Example2 a
--
-- -- hmm (Example1 list) = encode list
--
-- temp = Example1 []
--
-- -- example1 = Example1 []
-- example2 [] = Example2 @[()] []
-- example2 a = Example2 a

-- test = example2 []

-- the only problem is with the parent type being ambiguous
-- is there any way to solve this?

-- data EmptyOrAny b where
--   Empty :: a -> EmptyOrAny [()]
--   Any   :: FromJSON a => a -> EmptyOrAny a
--
-- another :: a -> ()
-- another _ = ()
--
-- -- mkEmptyOrAny :: [a] -> EmptyOrAny b
-- mkEmptyOrAny [] = Empty []
-- mkEmptyOrAny [a]  = Any [a]

type Not :: Bool -> Bool
type family Not a where
  Not True = False
  Not False = True

type Identity :: Type -> Type
type family Identity t where
  Identity t = t

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf True  t = Maybe t
  MaybeIf False t = Identity t

data Packet where
  Packet :: Applicative a => { interpreter :: Proxy a, stuff :: String } -> Packet

-- if the
test
  :: forall a b f. (Typeable a, Typeable b)
  => Either a b -> Integer
test x
    | typeOf (undefined :: a) == typeOf (undefined :: a) = 1
    | otherwise = 2

defaultPacket = Packet (Proxy :: Proxy IO) "hello"

-- so there could be a default... that's overwritten once it's embedded in a tree?
data Tree action m where

-- provided by class Num a => Integral a
-- integralIsNum :: Integral a -> Num a

-- provided by definition of class Num a
-- (+) :: forall a. Num a => a -> a -> a
-- fromInteger :: forall a. Num a => Integer -> a

-- func :: forall a. Integral a => a -> a -> a
-- func @a iDict x y = (+) @a (integralIsNum iDict) x y
--
-- func' :: forall a. Integral a => a -> a
-- func' @a iDict = func @a iDictA (f @a (integralIsNum iDict))
--
-- f :: forall a. Num a => a
-- f @a nDict = fromInteger @a nDict (2::Integer)

-- so what if I had a Maybe Proxy?

-- x :: forall {b} a. [Either a b] -> [Either a b]
-- x y = y



-- f zoop = if null zoop then x @() [Right "hey"] else x @String zoop

-- fromRights = concatMap getRight
--   where getRight (Right x) = [x]
--         getRight _         = []
--
-- toJson :: (Show a, ToJSON a) => [a] -> [ByteString]
-- toJson = fmap encode
--
-- test = toJson [Right "str"]
--
-- zed (Left a) = encode a
-- zed (Right a) = encode a
--
-- data DirectedEvent a b where
--   Parent :: a -> DirectedEvent a b
--   Self :: b -> DirectedEvent a b
--   deriving (Generic, Show, Eq)
--
-- zedd (Parent x) = encode x
-- zedd (Self x) = encode x
--
-- test2 = toJson [Parent "str"]
--
-- instance (ToJSON a, ToJSON b) => ToJSON (DirectedEvent a b) where
--   toEncoding = genericToEncoding defaultOptions
--
-- -- collect = toJson . fromRights
--
-- next items =
--   if any isLeft items
--   then items
--   else Left () : items
--
-- -- fine fuck it
-- doIt :: [Either a b] -> Either [b] [Either a b]
-- doIt items =
--   if any isLeft items
--   then Right items
--   else Left (fromRights items)

-- fixEither items =
--   if any isLeft items
--   then items
--   else items @(forall a b c. a (Either b c))

-- encode' :: ToJSON a => [a] -> String
-- encode' = encode

-- toEither :: [a] -> Either () [a]
-- simple [] = [] @()
-- simple a  = a

-- right, the list of events could have messages to self, but none
-- for the parent

-- addDefaultType :: [a] -> [a]
-- addDefaultType a = if null a then [] @() else a

-- so can I get example2 to work with some kind of in between?
-- very interesting and weird
-- mkExample :: (FromJSON a => [a]) -> Example
-- mkExample = Example1

-- data Wrap where
--   MkExample2 :: { list3 :: FromJSON a => [a] } -> Wrap

-- can I go from the higher level to the more concrete level?

-- data Test where
--   Test :: { a :: ToJSON x => x, b :: x } -> Test

-- mk = Test "" ()
