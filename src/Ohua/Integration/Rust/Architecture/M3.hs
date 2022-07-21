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
    convertExp,
    convertPat,
    convertStmt,
    noSpan,
    prependToBlock,
  )
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import qualified Ohua.Integration.Rust.TypeExtraction as TE
import Ohua.Integration.Rust.Types as RT
import Ohua.Prelude

instance Architecture (Architectures 'M3) where
  type Lang (Architectures 'M3) = Language 'Rust
  type Chan (Architectures 'M3) = Rust.Stmt ()
  type ATask (Architectures 'M3) = Rust.Expr ()

  convertChannel SM3 (SRecv _ty (SChan bnd)) =
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
                [ Sub.IdentPat Sub.Immutable $ bnd <> "_tx",
                  Sub.IdentPat Sub.Immutable $ bnd <> "_rx"
                ]
          )
          Nothing
          (Just channel)
          []
          noSpan

  convertRecv SM3 (SRecv TypeVar (SChan _channel)) = error "Invariant broken!"
  -- QUESTION: Can we use the same pattern here as for STM? I'll just use it for now to keep on working with the test code. 
  convertRecv SM3 (SRecv (Type (TE.Self ty _ _mut)) (SChan channel)) =
    let ty' = noSpan <$ ty
        send =
          Sub.MethodCall
            (Sub.Var $ channel <> "_rx")
            (Sub.CallRef (asQualBind "recv_msg") $ Just $ Sub.AngleBracketed [Sub.TypeArg $ Sub.RustType ty'])
            []
    in Sub.MethodCall send (Sub.CallRef (asQualBind "unwrap") Nothing) []
    -- error "Not yet implemented: Recv of reference type"
  convertRecv SM3 (SRecv (Type (TE.Normal ty)) (SChan channel)) =
    let ty' = noSpan <$ ty
        send =
          Sub.MethodCall
            (Sub.Var $ channel <> "_rx")
            (Sub.CallRef (asQualBind "recv_msg") $ Just $ Sub.AngleBracketed [Sub.TypeArg $ Sub.RustType ty'])
            []
     in Sub.MethodCall send (Sub.CallRef (asQualBind "unwrap") Nothing) []
  -- ISSUE: Find correct reaction here
  convertRecv SM3 (SRecv (TupleTy lst_of_types) (SChan channel)) = undefined

  -- QUESTION: Why is sending a binding (Right binding) undefined and (Left Lit ty) is ok?
  -- REMINDER: To make progress in testing stuff I'll adopt the handling from STM here. Check if
    -- thats valid and replace otherwise 
  convertSend SM3 (SSend (SChan channel) toSend) = case toSend of
    Right num@NumericLit{} -> asMethodCall $ Sub.Lit num
    Right b@BoolLit{} -> asMethodCall $ Sub.Lit b
    Right s@StringLit{} -> asMethodCall $ Sub.Lit s
    Right UnitLit -> asMethodCall $ Sub.Lit UnitLit
    Right (EnvRefLit bnd) -> asMethodCall $ Sub.Var bnd
    Right (FunRefLit _) -> error "Invariant broken: Got tasked to send a function reference via channel which should have been caught in the backend."
    (Left d) -> asMethodCall $ Sub.Var d
    where 
      asMethodCall item = 
        Sub.MethodCall
              (Sub.MethodCall (Sub.Var $ channel <> "_tx") (Sub.CallRef (asQualBind "send_msg") Nothing) [item])
              (Sub.CallRef (asQualBind "unwrap") Nothing)
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
                convertExp $
                  prependToBlock (activateCom task) $
                    Sub.BlockExpr taskE
            all = map convertStmt initVPE ++ [Rust.NoSemi taskE' noSpan]
         in Rust.BlockExpr [] (Rust.Block all Rust.Normal noSpan) Nothing noSpan <$ task

      createVPE :: Sub.Stmt
      createVPE =
        Sub.Local (Sub.IdentP $ Sub.IdentPat Sub.Mutable "vpe") Nothing $
          Sub.MethodCall
            (Sub.Call (Sub.CallRef (QualifiedBinding (makeThrow ["VPE"]) "new_child_vpe") Nothing) []) -- TODO would normally take some argument
            (Sub.CallRef (asQualBind "unwrap") Nothing)
            []

      activateCom :: FullTask TE.RustTypeAnno a -> [Sub.Stmt]
      activateCom (FullTask sends recvs _) =
        map
          ( Sub.Semi
              . ( \c ->
                    Sub.MethodCall
                      (Sub.MethodCall (Sub.Var c) (Sub.CallRef (asQualBind "activate") Nothing) [])
                      (Sub.CallRef (asQualBind "unwrap") Nothing)
                      []
                )
          )
          ( map (\(SSend (SChan c) _) -> c) sends
              ++ map (\(SRecv _type (SChan c)) -> c) recvs
          )

      delegateCom :: FullTask TE.RustTypeAnno a -> [Sub.Stmt]
      delegateCom (FullTask sends recvs _) =
        map
          ( Sub.Semi
              . ( \c ->
                    Sub.MethodCall
                      ( Sub.MethodCall
                          (Sub.Var "vpe")
                          (Sub.CallRef (asQualBind "delegate_obj") Nothing)
                          [Sub.MethodCall (Sub.Var c) (Sub.CallRef (asQualBind "sel") Nothing) []]
                      )
                      (Sub.CallRef (asQualBind "unwrap") Nothing)
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
                (convertCallRef $ Sub.CallRef (QualifiedBinding (makeThrow ["Box"]) "new") Nothing)
                [closure]
                noSpan
            run =
              Rust.MethodCall
                []
                (convertExp $ Sub.Var "vpe")
                (Rust.PathSegment (mkIdent "run") Nothing noSpan)
                [box]
                noSpan
         in Rust.MethodCall
              []
              run
              (Rust.PathSegment (mkIdent "unwrap") Nothing noSpan)
              []
              noSpan

  -- REMINDER: Replace Placeholder
  serialize SM3 mod placeholder ns = C.serialize mod ns createProgram placeholder
    where
      -- QUESTION: Will here ever be a Try Expr or is it just an STM Thing?
      createProgram (Program chans resultExpr tasks) = case resultExpr of
       (Sub.Try resultExpr') -> 
        let taskStmts = map (flip Rust.Semi noSpan . taskExpression) tasks
            program = toList chans ++ taskStmts
         in Rust.Block (program ++ [Rust.NoSemi (convertExp resultExpr') noSpan]) Rust.Normal noSpan
       anyExpr ->
        let taskStmts = map (flip Rust.Semi noSpan . taskExpression) tasks
            program = toList chans ++ taskStmts
         in Rust.Block (program ++ [Rust.NoSemi (convertExp anyExpr) noSpan]) Rust.Normal noSpan

asQualBind :: Binding -> QualifiedBinding
asQualBind = QualifiedBinding (makeThrow [])

instance Transform (Architectures 'M3)

