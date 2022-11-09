{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Subset
import Ohua.Integration.Transform.DataPar (concat, joinFuture, spawnFuture, takeN)
import Ohua.Integration.Options (Options(..))
import Ohua.Prelude hiding (concat)

runtime = "rt"

spawnWork :: Architectures 'SharedMemory -> Block -> Block
spawnWork (SSharedMemory Options{..}) block =
  let (RustBlock unsafety blockExpr', par) = runState (transformExprInBlockM go block) False
   in case par of
        True ->
          let rt =
                Local (IdentP $ IdentPat Immutable runtime) Nothing $
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
                                [Lit $ NumericLit $ fromMaybe 1 dataPar]
                            )
                            (CallRef (mkFunRefUnqual "build") Nothing)
                            []
                        )
                        (CallRef (mkFunRefUnqual "unwrap") Nothing)
                        []
                    ]
           in RustBlock unsafety (rt : blockExpr')
        False -> RustBlock unsafety blockExpr' 
  where
    -- (fun:rt:args') -> -- would be cleaner
    go (Call (CallRef f _) (Lit (FunRefLit (FunRef qb _ _)) : args))
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
        Semi $ MethodCall (Var runtime) (CallRef (mkFunRefUnqual "spawn") Nothing) [Var "work"],
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
