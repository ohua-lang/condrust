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
pub fn split_evenly<T>(mut ds: Vec<T>, taskcount: usize) -> Vec<Vec<T>> {
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
spawnWork (Block blockExpr d s) = Block (concat $ map go blockExpr) d s
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
        "spawn" -> undefined
              -- simple rename
        "collectFuture" ->
          [Local p t
           (Just $ Call
            a
            (convertExpr SSharedMemory
              $ B.Lit $ FunRefLit $ FunRef "/collect_work" Nothing Untyped)
            args
            noSpan) atts noSpan]
        _ -> [noSpan <$ e]
    go e = [noSpan <$ e]
    blockStmts = let (Block stmts _ _) = spawnBlock
                 in stmts
    spawnBlock = noSpan <$ [block|
// define this and append via prependtoBlock
//  let f = move || {};
{
    let mut handles = Vec::with_capacity(worklist.len());

    for lst in worklist.drain(..) {
        let (sx, rx) = mpsc::channel();

        rt.spawn(async move { sx.send(f()).unwrap() });

        handles.push(rx);
    }

    (rt, handles)
}
|]

createRuntime :: SourceFile ()
createRuntime = noSpan <$ [sourceFile|
pub fn create_runtime(threadcount: usize) -> Arc<Runtime> {
    Arc::new(
        Builder::new()
            .threaded_scheduler()
            .core_threads(threadcount)
            .thread_name("ohua-tokio-worker")
            .build()
            .unwrap(),
    )
}
|]

collectFuture :: SourceFile ()
collectFuture = noSpan <$ [sourceFile|
 pub fn collect_future<T>(future : Receiver<T>) -> T {
   future.recv().unwrap()
}
|]
