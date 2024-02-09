{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

--import Ohua.Integration.Rust.Architecture.SharedMemory ()
import qualified Ohua.Integration.Rust.Types.Extraction as RT
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Subset
import Ohua.Integration.Rust.Common.Subset (TyRef(..), GenericArgs(..), RustType(..), GenericArg(..))
import Ohua.Integration.Rust.Backend.Convert (convertTyRef)
import Ohua.Integration.Transform.DataPar (concat, joinFuture, spawnFuture, takeN)
import Ohua.Integration.Options (Options(..))
import Ohua.Commons.Prelude hiding (concat)

runtime = "rt"

liftCollectType :: RT.RustVarType -> RT.RustVarType
liftCollectType (RT.Normal t) =
    RT.Normal
    $ convertTyRef
    $ TyRef "std.sync.mpsc/Receiver"
    $ Just
    $ AngleBracketed [TypeArg $ RustType t]
liftCollectType t = t


spawnCall = MethodCall (Var runtime) (CallRef (mkFunRefUnqual "spawn") Nothing) [Var "work"]

getInitializers :: Architectures 'SharedMemory -> Block -> Maybe (Stmt, Stmt)
getInitializers (SSharedMemory Options{..}) block =
  case dataPar of
    Just parNum ->
      case isSpawn block of
        True ->
          let
            rt_arc = runtime <> "_arc"
            rt = IdentP $ IdentPat Immutable $ runtime
            rt_arc_stmt =
                Local (IdentP $ IdentPat Immutable rt_arc) Nothing $
                  Call
                   (CallRef "std.sync.Arc/new" Nothing)
                   [ MethodCall
                     ( MethodCall
                       ( MethodCall
                         ( MethodCall
                           (Call (CallRef "tokio.runtime.Builder/new" Nothing) [])
                           (CallRef (mkFunRefUnqual "threaded_scheduler") Nothing)
                           []
                         )
                         (CallRef (mkFunRefUnqual "core_threads") Nothing)
                         [Lit $ NumericLit parNum]
                       )
                       (CallRef (mkFunRefUnqual "build") Nothing)
                       []
                     )
                     (CallRef (mkFunRefUnqual "unwrap") Nothing)
                     []
                   ]
            rt_stmt = Local rt Nothing $
                        MethodCall
                          (Var rt_arc)
                          (CallRef (mkFunRefUnqual "clone") Nothing)
                          []
          in Just (rt_arc_stmt, rt_stmt)
        False -> Nothing
    Nothing -> Nothing
  where
    isSpawn (RustBlock _ stmts) =
      let exprs = catMaybes $ map expr stmts
      in case  [True | e <- exprs, e' <- universe e, e' == spawnCall] of
           [] -> or $ map isSpawn $ catMaybes $ map blck [e' | e <- exprs, e' <- universe e]
           _ -> True

    expr (Semi e) = Just e
    expr (NoSemi e) = Just e
    expr (Local _ _ e) = Just e
    expr _ = Nothing

    blck (Loop b) = Just b
    blck (ForLoop _ _ b) = Just b
    blck (While _ b) = Just b
    blck (BlockExpr b) = Just b
    blck (If _ b _) = Just b
    blck (Async b) = Just b
    blck _ = Nothing


spawnWork :: Architectures 'SharedMemory -> Block -> Block
spawnWork (SSharedMemory Options{..}) block =
  case dataPar of
    Just _ ->
      let (RustBlock unsafety blockExpr', _par) = runState (transformExprInBlockM go block) False
      in RustBlock unsafety blockExpr'
    Nothing -> block
  where
    -- (fun:rt:args') -> -- would be cleaner
    go (Call (CallRef f _) (Lit (FunRefLit (FunRef qb _)) : args))
      | f == spawnFuture = do
        modify $ const True
        return $ BlockExpr $ RustBlock Normal $ spawnComp $ Call (CallRef qb Nothing) args
    go (Call (CallRef f _) [future])
      | f == joinFuture =
        pure $
          MethodCall
            (MethodCall future (CallRef (mkFunRefUnqual "recv") Nothing) [])
            (CallRef (mkFunRefUnqual "unwrap") Nothing)
            []
    go e = pure e

    -- {
    --  let (tx, rx) = mpsc::channel();}
    --  let work = async { tx.send(f(arg1,...,argn)).unwrap() };
    --  rt.spawn(work);
    --  rx
    -- }
    spawnComp comp =
      [ Local (TupP [IdentPat Immutable "tx", IdentPat Immutable "rx"]) Nothing $
          Call (CallRef "std.sync.mpsc/channel" Nothing) [],
        Local (IdentP $ IdentPat Immutable "work") Nothing $
          Async $
            RustBlock
              Normal
              [ NoSemi $
                  MethodCall
                    (MethodCall (Var "tx") (CallRef (mkFunRefUnqual "send") Nothing) [comp])
                    (CallRef (mkFunRefUnqual "unwrap") Nothing)
                    []
              ],
        Semi $ spawnCall,
        NoSemi $ Var "rx"
      ]

amorphous :: Block -> Block
amorphous = transformExprInBlock go
  where
    go (Call (CallRef f _) [v, n])
      | f == takeN =
        BlockExpr $
          RustBlock
            Normal
            [ Local
                (IdentP $ IdentPat Immutable "sp")
                Nothing
                ( If
                    (Binary Lt (MethodCall v (CallRef (mkFunRefUnqual "len") Nothing) []) n)
                    (RustBlock Normal [NoSemi $ MethodCall v (CallRef (mkFunRefUnqual "len") Nothing) []])
                    $ Just n
                ),
              Local
                (IdentP $ IdentPat Immutable "chunk")
                Nothing
                (MethodCall v (CallRef (mkFunRefUnqual "split_off") Nothing) [Var "sp"]),
              NoSemi (Tuple [v, Var "chunk"])
            ]
    go (Call (CallRef f _) [results, rest])
      | f == concat =
        BlockExpr $
          RustBlock
            Normal
            [ Semi $
                MethodCall
                  results
                  (CallRef (mkFunRefUnqual "extend") Nothing)
                  [MethodCall rest (CallRef (mkFunRefUnqual "into_iter") Nothing) []], -- assumes Vec
              NoSemi results
            ]
    go e = e
