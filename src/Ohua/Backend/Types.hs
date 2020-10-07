module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Backend.Lang

import qualified Data.ByteString.Lazy.Char8 as L


data TCProgram chan expr = 
    TCProgram 
        [chan] -- ^ Channels
        (Com 'Recv) -- ^ Result channel
        [expr] -- ^ Tasks
        -- [Function expr] -- ^ Functions
        deriving (Show, Eq)

class Integration lang where
    type Code lang :: *
    
    lower :: 
        CompM m
        => lang
        -> Namespace (TCProgram (Com 'Channel) TaskExpr)
        -> m (Namespace (TCProgram (Com 'Channel) (Code lang)))

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
        -> Namespace (TCProgram (Com 'Channel) (Code (Integ arch)))
        -> m (Namespace (TCProgram (Chan arch) (Task arch)))

    serialize :: 
        CompM m
        => arch
        -> Integ arch
        -> Namespace (TCProgram (Chan arch) (Task arch))
        -> m (NonEmpty (FilePath, L.ByteString))
