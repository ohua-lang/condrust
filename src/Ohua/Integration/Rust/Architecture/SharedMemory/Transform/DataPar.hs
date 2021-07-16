{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

import Ohua.Prelude
import Ohua.Backend.Types as BT
import Ohua.Frontend.Lang as F
import Ohua.Backend.Lang as B
import Ohua.Integration.Rust.Frontend
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Architecture.SharedMemory

import Language.Rust.Quote
import Language.Rust.Syntax
import Language.Rust.Parser (Span)

--modules = undefined

splitEvenly :: SourceFile Span
splitEvenly = [sourceFile|
/// Splits the input vector into evenly sized vectors for `taskcount` workers.
pub fn split_evenly<T>(mut ds : Vec<T>, taskcount: usize) -> Vec<Vec<T>> {
    let l = ds.len() / taskcount;
    let mut rest = ds.len() % taskcount;

    let mut splits = vec![Vec::with_capacity(l); taskcount];

    for t_num in 0..taskcount {
        if rest > 0 {
            splits[t_num] = ds.split_off(ds.len() - l - 1);
            rest -= 1;
        } else {
            if ds.len() <= l {
                splits[t_num] = ds.split_off(0);
            } else {
                splits[t_num] = ds.split_off(ds.len() - l);
            }
        }
    }

    splits
}
|]

-- FIXME somehow we really need to get the expression to be executed onto the task.
--       the better approach would be to turn this into a closure!
spawnWork :: Block () -> Block ()
spawnWork (Block blockExpr d s) =
  let (pars, blockExpr') = unzip $ map go blockExpr
      blockExpr'' = concat blockExpr'
  in case or pars of
    True ->
      let rt =
            noSpan <$ [stmt|
                           let rt = Arc::new(
                           Builder::new()
                           .threaded_scheduler()
                           .core_threads(threadcount)
                           .thread_name("ohua-tokio-worker")
                           .build()
                           .unwrap(),
                           );
                           |]
      in Block (rt : blockExpr'') d s
    False -> Block blockExpr'' d s
  where
    go e@(Local p t
          (Just
           (Call a (PathExpr _ _ (Path _ [ PathSegment "ohua" _ _
                                         , PathSegment "lang" _ _
                                         , PathSegment fun _ _] _)
                     _) args _))
           atts _) =
      case fun of
        -- splice in call
        "spawn" ->
          case args of
            -- (fun:rt:args') -> -- would be cleaner
            (f:args') ->
              ( True
              , [Local p t
                 (Just $
                  BlockExpr
                   []
                   (Block (spawnStmts' $ Call [] f args' noSpan) Normal noSpan)
                   Nothing
                   noSpan)
                 [] noSpan
                ]
              )
            _ -> error "invariant broken"
        -- simple rename
        "collectFuture" ->
          ( False
          , [Local p t
             (Just $ Call
               a
               (convertExpr SSharedMemory
                 $ B.Lit $ FunRefLit $ FunRef "/collect_future" Nothing Untyped)
               args
               noSpan) atts noSpan]
          )
        _ -> (False, [noSpan <$ e])
    go e = (False, [noSpan <$ e])

      -- |
      -- {
      --  let comp = move || { f(arg1,...,argn)
      --  let (tx, rx) = mpsc::channel();}
      --  let work = async { tx.send(comp()).unwrap() };
      --  rt.spawn(work);
      --  rx
      -- }
    spawnStmts' comp =
      [ Local
        (mkSimpleBinding "comp")
        Nothing
        (Just $
          Closure [] Ref NotAsync Movable
          (FnDecl [] (Just $ Infer noSpan) False noSpan)
          comp
          noSpan)
        []
        noSpan
      , noSpan <$ [stmt| let (tx,rx) = std::sync::mpsc::channel(); |]
      , noSpan <$ [stmt| let work = async move { tx.send(comp()).unwrap() };|]
      , noSpan <$ [stmt| rt.spawn(work);|]
      , NoSemi (noSpan <$ [expr| rx |]) noSpan
      ]

collectFuture :: SourceFile ()
collectFuture = noSpan <$ [sourceFile|
 pub fn collect_future<T>(future : Receiver<T>) -> T {
   future.recv().unwrap()
}
|]
