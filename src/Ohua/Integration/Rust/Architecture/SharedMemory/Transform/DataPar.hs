{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar where

import Ohua.Prelude
import Ohua.Backend.Types as BT
import Ohua.Backend.Lang as B
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Architecture.SharedMemory ()

import Language.Rust.Quote
import Language.Rust.Syntax


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
              -- TODO swallow this invariant
            _ -> error "invariant broken"
        -- simple rename
        "collectFuture" ->
          ( False
          , [Local p t
             (Just $ Call
               a
               (noSpan <$ [expr| |future| future.recv().unwrap() |])
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
