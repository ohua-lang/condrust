{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
module Ohua.Types.Vector where

import Universum hiding (Nat, toList, map, replicate, zip, zip3, filter, unzip, unzip3)
import Data.Singletons

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

deriving instance Generic Nat
deriving instance Show Nat
deriving instance Eq Nat

instance Semigroup Nat where
  Zero <> n = n
  (Succ n1) <> n2 = Succ (n1 <> n2)

data Vec (n::Nat) a where
    VNil :: Vec 'Zero a
    (:>) :: a -> Vec n a -> Vec ('Succ n) a

-- Foundation for this: https://richarde.dev/papers/2021/exists/exists.pdf
data ExVec :: Type -> Type where
  MkEV :: Vec n a -> ExVec a

deriving instance (Show a) => Show (Vec n a)
deriving instance (Eq a) => Eq (Vec n a)
deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

-- computed by $(genSingletons [’’Nat])
-- Sing is an open type family (as opposed to a closed type family declared with a GADT)
-- data instance Sing (a :: Nat) where
--     SZero :: Sing 'Zero
--     SSucc :: (SingI n) => Sing n -> Sing ('Succ n)
-- type SNat (n ::Nat) = Sing n

-- Resources:
-- https://blog.jle.im/entry/introduction-to-singletons-2.html
-- https://github.com/goldfirere/singletons/tree/01fa5635d8e4db0be8d9537a44d777541969354b#arrows-nat-symbol-and-literals
-- https://github.com/goldfirere/singletons/issues/76
-- Singletons paper: https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf (some listings are out-dated - Sing is now a type family)

data SNat :: Nat -> Type where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)

type instance Sing = SNat

instance SingKind Nat where
    type Demote Nat = Nat

    fromSing :: Sing (s :: Nat) -> Nat
    fromSing SZero = Zero
    fromSing (SSucc n) = Succ $ fromSing n

    toSing :: Nat -> SomeSing Nat
    toSing Zero = SomeSing SZero
    toSing (Succ n) = case toSing n of
                        (SomeSing n') -> SomeSing $ SSucc n'

type family (a :: Nat) + (b :: Nat) :: Nat where
  'Zero + n = n
  'Succ n + m = 'Succ (n + m)

add :: SNat n -> SNat m -> SNat (n + m)
add SZero m = m
add (SSucc n) m = SSucc (add n m)

toNonEmpty :: Vec ('Succ n) a -> NonEmpty a
toNonEmpty (x :> xs) = x :| toList xs

toList :: Vec n a -> [a]
toList VNil = []
toList (a:>rest) = a : toList rest

fromList :: SNat n -> [a] -> Vec n a
fromList s xs =
    case (s,xs) of
        (SZero,[]) -> VNil
        (SSucc s', a:rest) -> a :> fromList s' rest

map :: (a -> b) -> Vec n a -> Vec n b
map _ VNil = VNil
map f (x :> xs) = f x :> map f xs

zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip VNil VNil = VNil
zip (x :> xs) (y :> ys) = (x, y) :> zip xs ys

unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip VNil = (VNil, VNil)
unzip ((x, y) :> xys) = case unzip xys of
  (xs, ys) -> (x :> xs, y :> ys)

zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a, b, c)
zip3 VNil VNil VNil = VNil
zip3 (x :> xs) (y :> ys) (z :> zs) = (x, y, z) :> zip3 xs ys zs

unzip3 :: Vec n (a, b, c) -> (Vec n a, Vec n b, Vec n c)
unzip3 VNil = (VNil, VNil, VNil)
unzip3 ((x, y, z) :> xyzs) = case unzip3 xyzs of
  (xs, ys, zs) -> (x :> xs, y :> ys, z :> zs)

filter :: (a -> Bool) -> Vec n a -> ExVec a
filter f VNil = MkEV VNil
filter f (x :> xs) = if f x then (case filter f xs of
                            MkEV v -> MkEV (x :> v)) else filter f xs

nlength :: [a] -> Nat
nlength [] = Zero
nlength (_:xs) = Succ $ nlength xs

replicate :: Nat -> a -> [a]
replicate Zero _ = []
replicate (Succ n) x = x : replicate n x

replicateNE :: SNat ('Succ n) -> a -> NonEmpty a
replicateNE (SSucc n) x = x :| replicate (fromSing n) x

withSing :: Nat -> (forall n. SNat n -> a) -> a
withSing n f = case toSing n of
                 SomeSing s -> f s

withSuccSing :: Nat -> (forall n.SNat ('Succ n) -> a) -> a
withSuccSing n f = case toSing n of
                     SomeSing s@(SSucc _) -> f s
                     

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

instance Traversable (Vec n ) where
  traverse f VNil = pure VNil
  traverse f (x :> xs) =  (:>) <$> f x <*> traverse f xs