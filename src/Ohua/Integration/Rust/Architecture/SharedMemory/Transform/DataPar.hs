{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

import Ohua.Prelude hiding (concat)
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Transform.DataPar (spawnFuture, joinFuture, takeN, concat)
import Ohua.Integration.Rust.Architecture.SharedMemory ()

import Language.Rust.Quote
import Language.Rust.Syntax
import Language.Rust.Data.Ident


spawnWork :: Block () -> Block ()
spawnWork (Block blockExpr d s) =
  let (pars, blockExpr') = unzip $ map (transformBlockExpr go) blockExpr
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
         in Block (rt : blockExpr') d s
       False -> Block blockExpr' d s
  where
    go e@(Local p t
          (Just
           (Call a (PathExpr _ _ path _) args _))
           atts _) =
      case convertPath path of
        (Just f) | f == spawnFuture ->
          case args of
            -- (fun:rt:args') -> -- would be cleaner
            (f:args') ->
              ( True
              , Local p t
                 (Just $
                  BlockExpr
                   []
                   (Block (spawnStmts' $ Call [] f args' noSpan) Normal noSpan)
                   Nothing
                   noSpan)
                 [] noSpan
              )
              -- TODO swallow this invariant
            _ -> error "invariant broken"
        (Just f) | f == joinFuture ->
          ( False
          , Local p t
             (Just $ Call
               a
               (noSpan <$ [expr| |future| future.recv().unwrap() |])
               args
               noSpan) atts noSpan
          )
        _ -> (False, noSpan <$ e)
    go e = (False, noSpan <$ e)

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

amorphous :: Block () -> Block ()
amorphous (Block blockExpr d s) = Block (snd $ unzip $ map (transformBlockExpr go) blockExpr) d s
  where
    go e@(Local p t
          (Just
           (Call a (PathExpr _ _ path _) args _))
           atts _) =
      case convertPath path of
        (Just f) | f == takeN ->
                   ( True
                   , Local p t
                     (Just
                      (Call
                       a
                       (noSpan <$ [expr| |v, n| v.split_at(n) |])
-- Version that only requires coll to implement iterable.
-- But we cannot recover the original type in concat anyhow.
--               [expr|
--                    |coll, n|
--                    {
--                      let mut taken = Vec::new();
--                      let mut rest = Vec::new();
--                      let mut i = 0;
--                      for c in coll {
--                        if i < n {
--                          taken.push(c);
--                        } else {
--                          rest.push(c);
--                        }
--                      }
--                      (taken, rest)
--                    }
--                    |])
                       args
                       noSpan)
                     )
                     atts
                     noSpan
                   )
        (Just f) | f == concat ->
                   ( True
                   , Local p t
                     (Just
                      (Call
                       a
                       (noSpan <$ [expr|
                                       move |results, rest|
                                       {
                                         results.append(rest);
                                         results
                                       }
                                       |])
                       args
                       noSpan)
                     )
                     atts
                     noSpan
                   )
        _ -> (False ,e)
    go e = (False, e)

convertPath :: Path a -> Maybe QualifiedBinding
convertPath (Path _ [o,l,f] _) = do
  o' <- fromString <$> convertSegment o
  l' <- fromString <$> convertSegment l
  f' <- fromString <$> convertSegment f
  pure $ QualifiedBinding (makeThrow [o', l']) f'
convertPath _ = Nothing

convertSegment :: PathSegment a -> Maybe Name
convertSegment (PathSegment Ident{name=n} Nothing _) = Just n
convertSegment _ = Nothing

-- FIXME this is a hack for now that works only for the above cases in this module.
-- FIXME we need a good traversal here. the fix is again to be on our own Rust data structure first!
-- FIXME should probably be monadic then applying f would just be fmap.
transformBlockExpr :: (Stmt () -> (Bool, Stmt ())) -> Stmt () -> (Bool, Stmt ())
transformBlockExpr f (Semi (Loop atts (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, Semi (Loop atts (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f (Semi (ForLoop atts pat e (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, Semi (ForLoop atts pat e (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f (Semi (While atts e (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, Semi (While atts e (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f (NoSemi (Loop atts (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, NoSemi (Loop atts (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f (NoSemi (ForLoop atts pat e (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, NoSemi (ForLoop atts pat e (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f (NoSemi (While atts e (Block blockExprs d s0) l s1) s2) =
  let (pars, blockExprs') = unzip $ map (transformBlockExpr f) blockExprs
  in (or pars, NoSemi (While atts e (Block blockExprs' d s0) l s1) s2)
transformBlockExpr f e = f e
