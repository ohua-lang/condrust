{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Python.Backend


import qualified Language.Python.Common.AST as Py


instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python 
    type Chan (Architectures 'MultiProcessing) = Py.Statement ()
    type ATask (Architectures 'MultiProcessing) = Py.Expr ()


-- Question: In the backend definition a task is a block/suite, while in the architecture it's just an Expression. Why that different levels?

--  convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch
    convertChannel SMultiProc (SChan bnd)= 
        let stmt = Apply $ 
                    Stateless 
                        -- Todo: replace with real QB as soon as I got ns extraction from 
                        -- imports right
                        (QualifiedBinding (makeThrow []) "mp.Pipe") 
                        []
            send = convertExpr SMultiProc $ TCLang.Var $ bnd <> "_send"
            recv = convertExpr SMultiProc $ TCLang.Var $ bnd <> "_recv"
        in Py.Assign {
             assign_to = [Py.Tuple [send, recv] noSpan],
             assign_expr = convertExpr SMultiProc stmt,
             stmt_annot = noSpan}
             
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
    -- Wie wird eine 'Task' (Py.Suite) ausgeführt..
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
    -- Generiert Python Code aus dem neuen AST 
    -- Alles was auf verteilten physischen Konten laufen soll, sollte auch in verschiedene Dateien.
    -- Für Multiprocessing reicht erstmal eine Datei, aber zB bei CloudMirkoservices währen Tasks deutlich unabhängiger 
    -- (eigene Module, eigene Imports etc.)
    serialize = undefined 


