module Ohua.Backend.Types where

import Ohua.Prelude

import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L


data TCProgram chan retChan expr = 
    TCProgram 
        (NonEmpty chan) -- ^ Channels
        retChan -- ^ Receive on result channel
        [expr] -- TODO (NonEmpty expr) -- ^ Tasks
        -- [Function expr] -- ^ Functions
        deriving (Show, Eq)

class Integration lang where
    type RetChan lang :: *
    type Expr lang :: *
    type Task lang :: *

    convertExpr :: (Architecture arch, Lang arch ~ lang) => arch -> TaskExpr -> Expr lang

    lower :: 
        ( CompM m
        , Architecture arch
        , Lang arch ~ lang)
        => lang
        -> arch
        -> Namespace (TCProgram (Chan arch) TaskExpr TaskExpr)
        -> m (Namespace (TCProgram (Chan arch) (RetChan lang) (Task lang)))

class (ConvertTaskCom arch) => Architecture arch where
    type Lang arch :: *
    type Chan arch :: *
    type ARetChan arch :: *
    type ATask arch :: *

    convertChannel :: arch -> Channel -> Chan arch

    build :: 
        ( Integration (Lang arch)
        , lang ~ (Lang arch)
        , CompM m)
        => arch
        -> lang
        -> Namespace (TCProgram (Chan arch) (RetChan lang) (Task lang))
        -> m (Namespace (TCProgram (Chan arch) (ARetChan arch) (ATask arch)))

    serialize :: 
        ( CompM m
        , Integration (Lang arch)
        , lang ~ (Lang arch))
        => arch
        -> lang
        -> Namespace (TCProgram (Chan arch) (ARetChan arch) (ATask arch))
        -> m (NonEmpty (FilePath, L.ByteString))

class ConvertTaskCom arch where
    convertRecv :: arch -> Com 'Recv -> TaskExpr
    convertSend:: arch -> Com 'Send -> TaskExpr
