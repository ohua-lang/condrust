{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

import Ohua.Integration.Rust.Architecture.SharedMemory ()
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Subset
import Ohua.Integration.Transform.DataPar (concat, joinFuture, spawnFuture, takeN)
import Ohua.Prelude hiding (concat)

-- TODO define via a configuration
threadCount = 1

runtime = "rt"

spawnWork :: Block -> Block
spawnWork block =
  let (Block blockExpr', par) = runState (transformExprInBlockM go block) False
   in case par of
        True ->
          let rt =
                Local (IdentP $ IdentPat Immutable runtime) $
                  Call
                    (CallRef "std.sync.Arc/new" Nothing)
                    [ MethodCall
                        ( MethodCall
                            ( MethodCall
                                ( MethodCall
                                    (Call (CallRef "tokio.runtime.Builder/new" Nothing) [])
                                    (CallRef "/threaded_scheduler" Nothing)
                                    []
                                )
                                (CallRef "/core_threads" Nothing)
                                [Lit $ NumericLit threadCount]
                            )
                            (CallRef "/build" Nothing)
                            []
                        )
                        (CallRef "/unwrap" Nothing)
                        []
                    ]
           in Block (rt : blockExpr')
        False -> Block blockExpr'
  where
    -- (fun:rt:args') -> -- would be cleaner
    go (Call (CallRef f _) (Lit (FunRefLit (FunRef qb _ _)) : args))
      | f == spawnFuture = do
        modify $ const True
        return $ BlockExpr $ Block $ spawnComp $ Call (CallRef qb Nothing) args
    go (Call (CallRef f _) [future])
      | f == joinFuture =
        pure $
          MethodCall
            (MethodCall future (CallRef "/recv" Nothing) [])
            (CallRef "/unwrap" Nothing)
            []
    go e = pure e

    -- {
    --  let (tx, rx) = mpsc::channel();}
    --  let work = async { tx.send(f(arg1,...,argn)).unwrap() };
    --  rt.spawn(work);
    --  rx
    -- }
    spawnComp comp =
      [ Local (TupP [IdentPat Immutable "tx", IdentPat Immutable "rx"]) $
          Call (CallRef "std.sync.mpsc/channel" Nothing) [],
        Local (IdentP $ IdentPat Immutable "work") $
          Async $
            Block
              [ NoSemi $
                  MethodCall
                    (MethodCall (Var "tx") (CallRef "/send" Nothing) [comp])
                    (CallRef "/unwrap" Nothing)
                    []
              ],
        Semi $ MethodCall (Var runtime) (CallRef "/spawn" Nothing) [Var "work"],
        NoSemi $ Var "rx"
      ]

amorphous :: Block -> Block
amorphous = transformExprInBlock go
  where
    go (Call (CallRef f _) [v, n])
      | f == takeN = MethodCall v (CallRef "/split_at" Nothing) [n] -- assumes Vec
    go (Call (CallRef f _) [results, rest])
      | f == concat =
        BlockExpr $
          Block
            [ Semi $ MethodCall results (CallRef "/append" Nothing) [rest], -- assumes Vec
              NoSemi results
            ]
    go e = e
