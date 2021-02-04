{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
module Ohua.Types.Vector where

import Universum hiding (Nat, toList, map)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons

data Nat where
    Zero :: Nat 
    Succ :: Nat -> Nat

deriving instance Show Nat

data Vec (n::Nat) a where
    VNil :: Vec 'Zero a
    (:>) :: a -> Vec n a -> Vec ('Succ n) a

deriving instance (Show a) => Show (Vec n a)

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
map f VNil = VNil
map f (x :> xs) = f x :> map f xs

nlength :: [a] -> Nat
nlength [] = Zero
nlength (x:xs) = Succ $ nlength xs