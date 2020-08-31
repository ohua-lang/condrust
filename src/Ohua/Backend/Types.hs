module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Backend.Lang
import Ohua.Backend.Convert

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as L


data TCProgram chan task = TCProgram [chan] Channel [task] deriving (Show, Eq)

class Integration lang where
    type Code lang :: *
    
    lower :: 
        CompM m
        => lang
        -> Namespace (TCProgram Channel TaskExpr)
        -> m (Namespace (TCProgram Channel (Code lang)))

class Architecture arch where
    type Integ arch :: *
    type Chan arch :: *
    type Task arch :: *

    build :: 
        ( CompM m
        , Integration (Integ arch)
        )
        => arch
        -> Integ arch
        -> Namespace (TCProgram Channel (Code (Integ arch)))
        -> m (Namespace (TCProgram (Chan arch) (Task arch)))

    serialize :: 
        CompM m
        => arch
        -> Integ arch
        -> Namespace (TCProgram (Chan arch) (Task arch))
        -> m (NonEmpty (FilePath, L.ByteString))
