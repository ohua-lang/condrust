-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ohua.Types where

import           Control.DeepSeq
import           Data.Hashable
import           Data.String
import           GHC.Exts
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util

newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord)

-- Only here so we can write literals and have them convert automatically
instance Num FnId where
    fromInteger = FnId . fromInteger

instance Show FnId where
    show = show . unFnId
instance Hashable FnId where hashWithSalt s = hashWithSalt s . unFnId
instance NFData FnId where rnf (FnId i) = rnf i

newtype Binding = Binding { unBinding :: String }
    deriving (Eq, Hashable)

instance Show Binding where show = unBinding
instance NFData Binding where rnf (Binding b) = rnf b


instance IsString Binding where
    fromString = either error (either (const $ error "Binding must not be fully qualified") id) . symbolFromString

data FnName = FnName
    { fnNameNamespace :: !String
    , fnNameName      :: !String
    } deriving (Eq, Ord)

instance HasName FnName String where name = lens fnNameName (\s a -> s {fnNameName=a})
instance NFData FnName where rnf (FnName ns n) = rnf ns `seq` rnf n
instance Hashable FnName where hashWithSalt s (FnName a b) = hashWithSalt s (a, b)

instance HasNamespace FnName String where namespace = lens fnNameNamespace (\s a -> s {fnNameNamespace=a})

instance Show FnName where
    show n
        | null (n^.name) = n^.namespace
        | otherwise = n^.namespace ++ "/" ++ n^.name

instance IsString FnName where
    fromString = either error (either id (const $ error "Function name must be fully qualified")) . symbolFromString

symbolFromString :: String -> Either String (Either FnName Binding)
symbolFromString [] = Left "Symbols cannot be empty"
symbolFromString s =
    case break (== '/') s of
        ([], _) -> Left "Unexpected '/' at start"
        (name,[]) -> Right $ Right $ Binding name
        (ns, '/':name)
            | '/' `elem` name -> Left "Too many '/' delimiters found."
            | otherwise -> Right $ Left $ FnName ns name
        _ -> error "Leading slash expected after `break`"


class ExtractBindings a where
    extractBindings :: a -> [Binding]
instance ExtractBindings a => ExtractBindings [a] where extractBindings = concatMap extractBindings


data Assignment
    = Direct !Binding
    | Destructure ![Binding]
    deriving (Eq)

instance Show Assignment where
    show (Direct b)      = show b
    show (Destructure d) = show d

instance IsString Assignment where
    fromString = Direct . fromString

instance IsList Assignment where
    type Item Assignment = Binding

    fromList = Destructure
    toList (Destructure l) = l
    toList _               = error "Direct return is not a list"

instance ExtractBindings Assignment where
    extractBindings (Direct bnd)       = [bnd]
    extractBindings (Destructure bnds) = bnds

instance NFData Assignment where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d)       = rnf d

_Direct :: Prism' Assignment Binding
_Direct = prism' Direct $ \case { Direct a -> Just a; _ -> Nothing }

_Destructure :: Prism' Assignment [Binding]
_Destructure = prism' Destructure $ \case { Destructure a -> Just a; _ -> Nothing }


flattenAssign :: Assignment -> [Binding]
flattenAssign = extractBindings
