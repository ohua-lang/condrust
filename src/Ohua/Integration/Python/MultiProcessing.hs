{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture


import qualified Language.Python.Common.AST as Py


instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python 
    type Chan (Architectures 'MultiProcessing) = Py.Statement ()
    type ATask (Architectures 'MultiProcessing) = Py.Expr ()


-- Question: In the backend definition a task is a block/suite, while in the architecture it's just an Expression. Why that different levels?

--  convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch
    convertChannel = undefined 
--  convertRecv :: arch -> Com 'Recv (Type (Lang arch)) -> Expr (Lang arch)
    convertRecv = undefined 
--  convertSend:: arch -> Com 'Send (Type (Lang arch)) -> Expr (Lang arch)
    convertSend = undefined 
{-  build :: 
        ( Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , expr ~ (Expr (Lang arch))
        , CompM m)
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) expr (Task lang) ty) (AlgoSrc lang)
        -> m (Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang))
-}
    build = undefined 

{-    serialize :: 
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
    serialize = undefined 


