{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Types as RT
import Ohua.Integration.Rust.Architecture.Common as C

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Quote
import Language.Rust.Data.Ident


instance Architecture (Architectures 'SharedMemory) where
    type Lang (Architectures 'SharedMemory) = Module
    type Chan (Architectures 'SharedMemory) = Stmt ()
    type ARetChan (Architectures 'SharedMemory) = Rust.Expr ()
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

    build SSharedMemory (Module (_, SourceFile _ _ _items)) ns = 
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (TCProgram chans retChan tasks) = 
                TCProgram chans (convertExpr SSharedMemory retChan) (map createTask tasks)

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
            createProgram (TCProgram chans resultExpr tasks) =
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
                    taskStmts = map (flip Semi noSpan . push . box) tasks
                    taskRunStmt = () <$ [stmt| run(tasks); |]
                    program = toList chans ++ [taskInitStmt] ++ taskStmts ++ [taskRunStmt]
                in Block (program ++ [NoSemi resultExpr noSpan]) Normal noSpan

instance ConvertTaskCom (Architectures 'SharedMemory) where
    convertRecv _ (SRecv (SChan channel)) =             
        Apply $ Stateful (channel <> "_rx") (mkFunRefUnqual "recv") []
    convertSend _ (SSend (SChan channel) d) =
        Apply $ Stateful (channel <> "_tx") (mkFunRefUnqual "send") [Var d]
