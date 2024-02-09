{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.SharedMemory where

import qualified Data.List as LS
import Language.Rust.Data.Ident (mkIdent)
import Language.Rust.Quote
import Language.Rust.Parser (Span)
import qualified Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Lang (Com (..), Channel)
import Ohua.Backend.Types hiding (convertExpr)
import Ohua.Integration.Architecture
import Ohua.Integration.Options
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
import qualified Ohua.Integration.Rust.Types.Extraction as TH
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar (getInitializers)
import Ohua.Commons.Prelude

instance Architecture (Architectures 'SharedMemory) where
  type Lang (Architectures 'SharedMemory) = Language 'Rust
  type Chan (Architectures 'SharedMemory) = Sub.Stmt
  type ATask (Architectures 'SharedMemory) = [Rust.Stmt ()]

  convertChannel :: Architectures 'SharedMemory -> Channel (Rust.Expr Span) TH.RustVarType -> Chan (Architectures 'SharedMemory)
  convertChannel SSharedMemory{} (SRecv argTy (SChan bnd)) =
    let chanTy =
          case convertToRustType argTy of
            Just rustType -> Just $
                Sub.AngleBracketed [ Sub.TypeArg rustType ]
            Nothing -> error $ "Couldn't type channel properly: Binding" <> show bnd <> "had type "<> show argTy
     in Sub.Local
          ( Sub.TupP
              [ Sub.IdentPat Sub.Immutable $ bnd <> "_tx",
                Sub.IdentPat Sub.Immutable $ bnd <> "_rx"
              ]
          )
          Nothing
          $ Sub.Call
            ( Sub.CallRef
                (QualifiedBinding (makeThrow ["std", "sync", "mpsc"]) "channel")
                chanTy
            )
            []

  convertRecv SSharedMemory{} (SRecv _type (SChan channel)) =
    Sub.Try $
      Sub.MethodCall
        (Sub.Var $ channel <> "_rx")
        (Sub.CallRef (mkFunRefUnqual "recv") Nothing)
        []
  convertSend SSharedMemory{} (SSend (SChan channel) d) = case d of
    Left bnd -> trySend $ Sub.Var bnd
    Right num@NumericLit{} -> trySend $ Sub.Lit num
    Right b@BoolLit{} -> trySend $ Sub.Lit b
    Right s@StringLit{} -> trySend $ Sub.Lit s
    Right UnitLit -> trySend $ Sub.Lit UnitLit
    Right hl@HostLit{} -> undefined -- This should crash when I introduced  host literal tests to remind me to implement it
    Right (EnvRefLit bnd _ty) -> trySend $ Sub.Var bnd
    Right (FunRefLit _) -> error "Invariant broken: Got tasked to send a function reference via channel which should have been caught in the backend."
    where
      trySend bnd = Sub.Try $
                    Sub.MethodCall
                    (Sub.Var $ channel <> "_tx")
                    (Sub.CallRef (mkFunRefUnqual "send") Nothing)
                    [bnd]

  build arch@SSharedMemory{} (TH.Module _ (Rust.SourceFile _ _ _items)) ns =
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
    where
      createTasksAndChannels (Program chans retChan tasks) =
        Program chans retChan (map (createTask <$>) tasks)

      createTask :: Sub.Block -> [Rust.Stmt ()]
      createTask code =
        -- TODO it would be better to have this in a type class, I suppose
        let initializers = getInitializers arch code
            task = Rust.Closure
                     []
                     Rust.Value
                     Rust.NotAsync
                     Rust.Movable
                     (Rust.FnDecl [] (Just $ Rust.Infer noSpan) False noSpan)
                     (Rust.BlockExpr [] (convertBlock code) Nothing noSpan)
                     noSpan
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
            task' = (push . box . replaceFinalStatement) task
        in case initializers of
             Nothing -> [Rust.Semi task' noSpan]
             Just (global,local) ->
               [ convertStmt global
               , flip Rust.Semi noSpan $
                   Rust.BlockExpr
                     []
                     (Rust.Block (convertStmt local : [Rust.NoSemi task' noSpan]) Rust.Normal noSpan)
                     Nothing
                     noSpan
               ]

  -- REMINDER: Replace placeholder
  serialize (SSharedMemory Options{..}) mod  placeholder ns  = C.serialize mod ns createProgram placeholder
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
            taskStmts = concatMap taskExpression tasks
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
      createProgram (Program chans expr tasks) = error $ "Compilations resulted in a result expression: " <> show expr <> "This is probably a bug, please report."

-- convertToRustType :: OhuaType TH.RustVarType Resolved -> Maybe Sub.RustType
convertToRustType rTy =  Sub.RustType <$> toRustTy rTy


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
