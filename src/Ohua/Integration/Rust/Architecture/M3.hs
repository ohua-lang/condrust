{-# LANGUAGE QuasiQuotes #-}

module Ohua.Integration.Rust.Architecture.M3 where

import Language.Rust.Data.Ident (mkIdent)
import Language.Rust.Quote
import qualified Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Lang (Com (..))
import Ohua.Backend.Types hiding (Expr, convertExpr)
import Ohua.Integration.Architecture
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Rust.Architecture.Common as C
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Convert
  ( convertCallRef,
    convertExpr,
    convertPat,
    convertStmt,
    noSpan,
    prependToBlock,
  )
import Ohua.Integration.Rust.Backend.Subset as Sub
import qualified Ohua.Integration.Rust.TypeExtraction as TE
import Ohua.Integration.Rust.Types as RT
import Ohua.Prelude

instance Architecture (Architectures 'M3) where
  type Lang (Architectures 'M3) = Language 'Rust
  type Chan (Architectures 'M3) = Rust.Stmt ()
  type ATask (Architectures 'M3) = Rust.Expr ()

  convertChannel SM3 (SChan bnd) =
    let channel =
          noSpan
            <$ [expr| {
                let mut rgate =
                  wv_assert_ok!(RecvGate::new(math::next_log2(256), math::next_log2(256)));
                let sgate =
                  wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
                (sgate, rgate)
            } |]
     in Rust.Local
          ( convertPat $
              Sub.TupP
                [ IdentPat Immutable $ bnd <> "_tx",
                  IdentPat Immutable $ bnd <> "_rx"
                ]
          )
          Nothing
          (Just channel)
          []
          noSpan

  convertRecv SM3 (SRecv TypeVar (SChan _channel)) = error "Invariant broken!"
  convertRecv SM3 (SRecv (Type (TE.Self _ty _ _mut)) (SChan _channel)) =
    error "Not yet implemented: Recv of reference type"
  convertRecv SM3 (SRecv (Type (TE.Normal ty)) (SChan channel)) =
    let ty' = noSpan <$ ty
        send =
          MethodCall
            (Var $ channel <> "_rx")
            (CallRef "/recv_msg" $ Just $ Rust.AngleBracketed [Rust.TypeArg ty'] [] noSpan)
            []
     in MethodCall send (CallRef "/unwrap" Nothing) []

  convertSend SM3 (SSend (SChan channel) d) =
    MethodCall
      (MethodCall (Var $ channel <> "_tx") (CallRef "/send_msg" Nothing) [Var d])
      (CallRef "/unwrap" Nothing)
      []

  build SM3 (Module _ (Rust.SourceFile _ _ _items)) ns =
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndRetChan)
    where
      createTasksAndRetChan (Program chans retChan tasks) =
        Program chans retChan (map create tasks)

      create task@(FullTask _ _ taskE) =
        let initVPE = createVPE : delegateCom task
            taskE' =
              createTask $
                convertExpr $
                  prependToBlock (activateCom task) $
                    BlockExpr taskE
            all = map convertStmt initVPE ++ [Rust.NoSemi taskE' noSpan]
         in Rust.BlockExpr [] (Rust.Block all Rust.Normal noSpan) Nothing noSpan <$ task

      createVPE :: Stmt
      createVPE =
        Local (IdentP $ IdentPat Mutable "vpe") $
          MethodCall
            (Call (CallRef "VPE/new_child_vpe" Nothing) []) -- TODO would normally take some argument
            (CallRef "/unwrap" Nothing)
            []

      activateCom :: FullTask TE.RustTypeAnno a -> [Stmt]
      activateCom (FullTask sends recvs _) =
        map
          ( Semi
              . ( \c ->
                    MethodCall
                      (MethodCall (Var c) (CallRef "/activate" Nothing) [])
                      (CallRef "/unwrap" Nothing)
                      []
                )
          )
          ( map (\(SSend (SChan c) _) -> c) sends
              ++ map (\(SRecv _type (SChan c)) -> c) recvs
          )

      delegateCom :: FullTask TE.RustTypeAnno a -> [Stmt]
      delegateCom (FullTask sends recvs _) =
        map
          ( Semi
              . ( \c ->
                    MethodCall
                      ( MethodCall
                          (Var "vpe")
                          (CallRef "/delegate_obj" Nothing)
                          [MethodCall (Var c) (CallRef "/sel" Nothing) []]
                      )
                      (CallRef "unwrap" Nothing)
                      []
                )
          )
          ( map (\(SSend (SChan c) _) -> c) sends
              ++ map (\(SRecv _typ (SChan c)) -> c) recvs
          )

      createTask :: Rust.Expr () -> Rust.Expr ()
      createTask code =
        let closure =
              Rust.Closure
                []
                Rust.Value
                Rust.NotAsync
                Rust.Movable
                (Rust.FnDecl [] (Just $ Rust.Infer noSpan) False noSpan)
                code
                noSpan
            box =
              Rust.Call
                []
                (convertCallRef $ CallRef "Box/new" Nothing)
                [closure]
                noSpan
            run =
              Rust.MethodCall
                []
                (convertExpr $ Var "vpe")
                (Rust.PathSegment (mkIdent "run") Nothing noSpan)
                [box]
                noSpan
         in Rust.MethodCall
              []
              run
              (Rust.PathSegment (mkIdent "unwrap") Nothing noSpan)
              []
              noSpan

  serialize SM3 mod ns = C.serialize mod ns createProgram
    where
      createProgram (Program chans (Try resultExpr) tasks) =
        let taskStmts = map (flip Rust.Semi noSpan . taskExpression) tasks
            program = toList chans ++ taskStmts
         in Rust.Block (program ++ [Rust.NoSemi (convertExpr resultExpr) noSpan]) Rust.Normal noSpan

instance Transform (Architectures 'M3)
