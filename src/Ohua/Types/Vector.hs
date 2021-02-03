{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Ohua.Types.Vector where

import Universum hiding (Nat, toList)
import Data.List.NonEmpty (NonEmpty(..))

data Nat where
    Zero :: Nat 
    Succ :: Nat -> Nat

deriving instance Show Nat

data Vec (n::Nat) a where
    VNil :: Vec 'Zero a
    VCons :: a -> Vec n a -> Vec ('Succ n) a

deriving instance (Show a) => Show (Vec n a)

toList :: Vec n a -> [a]
toList VNil = []
toList (VCons a rest) = a : toList rest

toNonEmpty :: (n ~ 'Succ m) => Vec n a -> NonEmpty a
toNonEmpty (VCons a rest) = a :| toList rest

-- fromNonEmpty :: (n ~ 'Succ m) =>  NonEmpty a -> Vec n a
-- fromNonEmpty (a :| rest) = VCons a $ fromList rest

-- fromList :: [a] -> Vec n a
-- fromList xs = case xs of 
--                 [] -> VNil
--                 (a:rest) -> VCons a $ fromList rest