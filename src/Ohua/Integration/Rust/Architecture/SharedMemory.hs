{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Types as RT
import Ohua.Integration.Rust.Architecture.Common as C

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Quote
import Language.Rust.Data.Ident


instance Architecture (Architectures 'SharedMemory) where
    type Lang (Architectures 'SharedMemory) = Language 'Rust
    type Chan (Architectures 'SharedMemory) = Stmt ()
    type ATask (Architectures 'SharedMemory) = Rust.Expr ()

    convertChannel SSharedMemory (SChan bnd) = 
        let stmt = Apply $ 
                    Stateless 
                        (QualifiedBinding (makeThrow ["std","sync","mpsc"]) "channel") 
                        []
        in Local
            (TupleP [mkSimpleBinding $ bnd <> "_tx", mkSimpleBinding $ bnd <> "_rx"] Nothing noSpan)
            Nothing
            (Just $ convertExpr SSharedMemory stmt)
            []
            noSpan

    convertRecv SSharedMemory (SRecv _type (SChan channel)) =             
        convertExpr SSharedMemory $
            Apply $ Stateful (Var $ channel <> "_rx") (mkFunRefUnqual "recv") []
    convertSend SSharedMemory (SSend (SChan channel) d) =
        convertExpr SSharedMemory $
            Apply $ Stateful (Var $ channel <> "_tx") (mkFunRefUnqual "send") [Var d]

    build SSharedMemory (Module _ (SourceFile _ _ _items)) ns = 
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (Program chans retChan tasks) = 
                Program chans retChan (map (createTask <$>) tasks)

            createTask :: Rust.Block () -> Rust.Expr ()
            createTask code = 
                Closure
                    []
                    Movable
                    Value
                    (FnDecl [] (Just $ Infer noSpan) False noSpan)
                    (BlockExpr [] code noSpan)
                    noSpan

    serialize SSharedMemory mod ns = C.serialize mod ns createProgram
        where
            createProgram (Program chans resultExpr tasks) =
                let taskInitStmt = noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
                    box task =
                        Call 
                            []
                            (PathExpr [] Nothing (convertQualBnd (QualifiedBinding (makeThrow ["Box"]) "new")) noSpan)
                            [task]
                            noSpan
                    push t =
                        MethodCall
                            []
                            (convertExpr SSharedMemory $ Var "tasks")
                            (mkIdent "push")
                            Nothing
                            [t]
                            noSpan
                    taskStmts = map (flip Semi noSpan . push . box . taskExpression) tasks
                    taskRunStmt = () <$ [stmt| run(tasks); |]
                    program = toList chans ++ [taskInitStmt] ++ taskStmts ++ [taskRunStmt]
                in Block (program ++ [NoSemi resultExpr noSpan]) Normal noSpan
