{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture



instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Error 
    type Chan (Architectures 'MultiProcessing) = Error 
    type ATask (Architectures 'MultiProcessing) = Error

-- ToDo: Make MultiProcessing an instance of Backend.
{-
class Architecture arch where
    type Lang arch :: *
    type Chan arch :: *
    type ATask arch :: *

    convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch
    convertRecv :: arch -> Com 'Recv (Type (Lang arch)) -> Expr (Lang arch)
    convertSend:: arch -> Com 'Send (Type (Lang arch)) -> Expr (Lang arch)

    build :: 
        ( Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , expr ~ (Expr (Lang arch))
        , CompM m)
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) expr (Task lang) ty) (AlgoSrc lang)
        -> m (Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang))

    serialize :: 
        ( CompM m
        , Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , expr ~ (Expr (Lang arch))
        )
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang)
        -> m (NonEmpty (FilePath, L.ByteString))
-}