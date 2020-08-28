module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Backend.Lang
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as L


data TCProgram chan task = TCProgram [chan] Channel [task] deriving (Show, Eq, Lift, Generic)

class Integration lang where
    type Code lang
    
    convert :: (CompM m, ConvertExpr (Code lang))
            => Namespace (TCProgram Channel TaskExpr)
            -> lang
            -> Namespace (TCProgram Channel (Code lang))

class Architecture arch where
    type Chan arch
    type Task arch

    build :: 
        ( CompM m
        , ConvertChannel (Chan arch)
        , Integration lang
        , code ~ Code lang)
        => Namespace (TCProgram Channel code)
        -> arch
        -> m (Namespace (TCProgram (Chan arch) (Task arch)))

    serialize :: 
        ( CompM m
        , ConvertChannel (Chan arch))
        => Namespace (TCProgram chan task)
        -> arch
        -> m (NonEmpty (FilePath, L.ByteString))
