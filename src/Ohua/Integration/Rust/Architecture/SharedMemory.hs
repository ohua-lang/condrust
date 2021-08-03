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

import qualified Data.List as LS

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
            (TupleP [mkSimpleBinding $ bnd <> "_tx", mkSimpleBinding $ bnd <> "_rx"] noSpan)
            Nothing
            (Just $ convertExpr SSharedMemory stmt)
            []
            noSpan

    convertRecv SSharedMemory (SRecv _type (SChan channel)) =
        Try [] (convertExpr SSharedMemory $
            Apply $ Stateful (Var $ channel <> "_rx") (mkFunRefUnqual "recv") []) noSpan
    convertSend SSharedMemory (SSend (SChan channel) d) =
        Try [] (convertExpr SSharedMemory $
            Apply $ Stateful (Var $ channel <> "_tx") (mkFunRefUnqual "send") [Var d]) noSpan

    build SSharedMemory (Module _ (SourceFile _ _ _items)) ns =
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (Program chans retChan tasks) =
                Program chans retChan (map (createTask <$>) tasks)

            createTask :: Rust.Block () -> Rust.Expr ()
            createTask code =
                Closure
                    []
                    Value
                    NotAsync
                    Movable
                    (FnDecl [] (Just $ Infer noSpan) False noSpan)
                    (BlockExpr [] code Nothing noSpan)
                    noSpan

    serialize SSharedMemory mod ns = C.serialize mod ns createProgram
        where
            createProgram (Program chans (Try _ resultExpr _) tasks) =
                let taskInitStmt = noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
                    (Block prelude _ _) = void [block| {
                                            #[derive(Debug)]
                                            enum RunError {
                                                SendFailed,
                                                RecvFailed
                                            }

                                            impl<T: Send> From<std::sync::mpsc::SendError<T>> for RunError {
                                                fn from(_err: std::sync::mpsc::SendError<T>) -> Self {
                                                    RunError::SendFailed
                                                }
                                            }

                                            impl From<std::sync::mpsc::RecvError> for RunError {
                                                fn from(_err: std::sync::mpsc::RecvError) -> Self {
                                                    RunError::RecvFailed
                                                }
                                            }
                                            }|]
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
                            (PathSegment (mkIdent "push") Nothing noSpan)
                            [t]
                            noSpan
                    taskStmts = map (flip Semi noSpan . push . box . replaceFinalStatement . taskExpression) tasks
                    (Block taskRunStmt _ _) = void [block|{
                                                         let mut handles: Vec<std::thread::JoinHandle<_>> = tasks.into_iter().map(|t| { std::thread::spawn(move || { let _ = t(); }) }).collect();
                                                         for h in handles {
                                                             if let Err(_) = h.join() {
                                                                 eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                                                             }
                                                         }
                                                         }|]
                    program = prelude ++ toList chans ++ [taskInitStmt] ++ taskStmts ++ taskRunStmt
                    resultHandling =
                      Match []
                      resultExpr
                      [ Arm [] (noSpan <$ [pat| Ok(res) |]) Nothing (noSpan <$ [expr| res |]) noSpan
                      , Arm [] (noSpan <$ [pat| Err(e) |]) Nothing (noSpan <$ [expr| panic!("[Ohua Runtime Internal Exception] {}", e) |]) noSpan
                      ]
                      noSpan
                in Block (program ++ [NoSemi resultHandling noSpan]) Normal noSpan

-- | Replaces the final Unit literal in a Rust operator with a `Ok(())` when the operator is *not* containing a loop.
replaceFinalStatement :: Rust.Expr () -> Rust.Expr ()
replaceFinalStatement e@(Closure oatt cb ia mv fun (BlockExpr att (Block blck safety ()) labels ()) ()) =
  case LS.last blck of
    (NoSemi (TupExpr [] [] _) _) -> let
      blck' = changeLast blck (NoSemi (noSpan <$ [expr| Ok(()) |]) noSpan)
      in
        (Closure oatt cb ia mv fun (BlockExpr att (Block blck' safety noSpan) labels noSpan) noSpan)
    _ -> e
  where
    changeLast :: [a] -> a -> [a]
    changeLast []     _  = error "Cannot change last element of an empty list"
    changeLast [_]    x  = [x]
    changeLast (x:xs) x' = x : changeLast xs x'
replaceFinalStatement e = e
