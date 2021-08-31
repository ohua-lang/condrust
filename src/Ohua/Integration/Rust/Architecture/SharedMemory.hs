{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.SharedMemory where

import qualified Data.List as LS
import Language.Rust.Data.Ident (mkIdent)
import Language.Rust.Quote
import qualified Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Lang (Com (..))
import Ohua.Backend.Types hiding (convertExpr)
import Ohua.Integration.Architecture
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Rust.Architecture.Common as C
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Convert
  ( convertBlock,
    convertExp,
    convertQualBnd,
    convertStmt,
    noSpan,
    convertGenericArgs
  )
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import qualified Ohua.Integration.Rust.TypeExtraction as TE
import Ohua.Integration.Rust.Types as RT
import Ohua.Prelude

instance Architecture (Architectures 'SharedMemory) where
  type Lang (Architectures 'SharedMemory) = Language 'Rust
  type Chan (Architectures 'SharedMemory) = Sub.Stmt
  type ATask (Architectures 'SharedMemory) = Rust.Expr ()

  convertChannel SSharedMemory (SRecv argTy (SChan bnd)) =
    -- help out the type inference of Rust a little here
    let chanTy =
          case argTy of
            TupleTy ts ->
              Just $
                Sub.AngleBracketed
                  [ Sub.TypeArg $ Sub.RustType $
                      Rust.TupTy (map (const (Rust.Infer noSpan)) $ toList ts) noSpan
                  ]
            Type (TE.Normal ti) ->
              Just $
                Sub.AngleBracketed
                  [Sub.TypeArg $ Sub.RustType (noSpan <$ ti)]
            Type (TE.Self ti _ _) ->
              Just $
                Sub.AngleBracketed
                  [Sub.TypeArg $ Sub.RustType (noSpan <$ ti)]
            _ -> Nothing
     in Sub.Local
          ( Sub.TupP
              [ Sub.IdentPat Sub.Immutable $ bnd <> "_tx",
                Sub.IdentPat Sub.Immutable $ bnd <> "_rx"
              ]
          )
          $ Sub.Call
            ( Sub.CallRef
                (QualifiedBinding (makeThrow ["std", "sync", "mpsc"]) "channel")
                chanTy
            )
            []

  convertRecv SSharedMemory (SRecv _type (SChan channel)) =
    Sub.Try $
      Sub.MethodCall
        (Sub.Var $ channel <> "_rx")
        (Sub.CallRef (mkFunRefUnqual "recv") Nothing)
        []
  convertSend SSharedMemory (SSend (SChan channel) d) =
    Sub.Try $
      Sub.MethodCall
        (Sub.Var $ channel <> "_tx")
        (Sub.CallRef (mkFunRefUnqual "send") Nothing)
        [Sub.Var d]

  build SSharedMemory (Module _ (Rust.SourceFile _ _ _items)) ns =
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
    where
      createTasksAndChannels (Program chans retChan tasks) =
        Program chans retChan (map (createTask <$>) tasks)

      createTask :: Sub.Block -> Rust.Expr ()
      createTask code =
        Rust.Closure
          []
          Rust.Value
          Rust.NotAsync
          Rust.Movable
          (Rust.FnDecl [] (Just $ Rust.Infer noSpan) False noSpan)
          (Rust.BlockExpr [] (convertBlock code) Nothing noSpan)
          noSpan

  serialize SSharedMemory mod ns = C.serialize mod ns createProgram
    where
      createProgram (Program chans (Sub.Try resultExpr) tasks) =
        let taskInitStmt =
              noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
            (Rust.Block prelude _ _) =
              void
                [block| {
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
              Rust.Call
                []
                ( Rust.PathExpr
                    []
                    Nothing
                    (convertQualBnd (QualifiedBinding (makeThrow ["Box"]) "new"))
                    noSpan
                )
                [task]
                noSpan
            push t =
              Rust.MethodCall
                []
                (convertExp $ Sub.Var "tasks")
                (Rust.PathSegment (mkIdent "push") Nothing noSpan)
                [t]
                noSpan
            taskStmts =
              map
                ( flip Rust.Semi noSpan
                    . push
                    . box
                    . replaceFinalStatement
                    . taskExpression
                )
                tasks
            (Rust.Block taskRunStmt _ _) =
              void
                [block|{
                let handles: Vec<std::thread::JoinHandle<_>> =
                  tasks.into_iter().map(|t| { std::thread::spawn(move || { let _ = t(); }) }).collect();
                for h in handles {
                    if let Err(_) = h.join() {
                        eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                    }
                }
                }|]
            chans' = map convertStmt $ toList chans
            program = prelude ++ chans' ++ [taskInitStmt] ++ taskStmts ++ taskRunStmt
            resultHandling =
              Rust.Match
                []
                (convertExp resultExpr)
                [ Rust.Arm [] (noSpan <$ [pat| Ok(res) |]) Nothing (noSpan <$ [expr| res |]) noSpan,
                  Rust.Arm [] (noSpan <$ [pat| Err(e) |]) Nothing (noSpan <$ [expr| panic!("[Ohua Runtime Internal Exception] {}", e) |]) noSpan
                ]
                noSpan
         in Rust.Block (program ++ [Rust.NoSemi resultHandling noSpan]) Rust.Normal noSpan

-- | Surrounds the final non-semicolon terminated statement in a Rust operator with a `Ok(...)` when the operator is *not* containing a loop.
replaceFinalStatement :: Rust.Expr () -> Rust.Expr ()
replaceFinalStatement e@(Rust.Closure oatt cb ia mv fun (Rust.BlockExpr att (Rust.Block blck safety ()) labels ()) ()) =
  case LS.last blck of
    (Rust.NoSemi lst _) -> case lst of
      Rust.Loop {} -> e
      _ ->
        let newStmt = Rust.Call [] (Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment "Ok" Nothing noSpan] noSpan) noSpan) [lst] noSpan
            blck' = changeLast blck (Rust.NoSemi newStmt noSpan)
         in Rust.Closure oatt cb ia mv fun (Rust.BlockExpr att (Rust.Block blck' safety noSpan) labels noSpan) noSpan
    _ -> e
  where
    changeLast :: [a] -> a -> [a]
    changeLast [] _ = error "Cannot change last element of an empty list"
    changeLast [_] x = [x]
    changeLast (x : xs) x' = x : changeLast xs x'
replaceFinalStatement e = e
