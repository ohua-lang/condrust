{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.M3 where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Rust.Types as RT
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Architecture.Common as C
import qualified Ohua.Integration.Rust.TypeExtraction as TE

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Data.Ident
import Language.Rust.Quote


instance Architecture (Architectures 'M3) where
    type Lang (Architectures 'M3) = Language 'Rust
    type Chan (Architectures 'M3) = Stmt ()
    type ATask (Architectures 'M3) = Rust.Expr ()

    convertChannel SM3 (SChan bnd) =
        let channel = noSpan <$ [expr| {
                let mut rgate = wv_assert_ok!(RecvGate::new(math::next_log2(256), math::next_log2(256)));
                let sgate = wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
                (rgate, sgate)
            } |]
        in Local
            (TupleP
              [mkSimpleBinding $ bnd <> "_tx", mkSimpleBinding $ bnd <> "_rx"]
              noSpan)
            Nothing
            (Just channel)
            []
            noSpan

    convertRecv SM3 (SRecv TypeVar (SChan _channel)) = error "Invariant broken!"
    convertRecv SM3 (SRecv (Type (TE.Self _ty _ _mut)) (SChan _channel)) = error "Not yet implemented: Recv of reference type"
    convertRecv SM3 (SRecv (Type (TE.Normal ty)) (SChan channel)) =
        let ty' = noSpan <$ ty
            send = MethodCall
                    []
                    (convertExpr SM3 $ Var $ channel <> "_rx")
                    (PathSegment
                      (mkIdent "recv_msg")
                      (Just $ AngleBracketed [TypeArg ty'] [] noSpan)
                      noSpan)
                    []
                    noSpan
        in unwrapMC send

    convertSend SM3 (SSend (SChan channel) d) =
        convertExpr SM3 $
            Apply $ Stateful
                (Apply $ Stateful (Var $ channel <> "_tx") (mkFunRefUnqual "send_msg") [Var d])
                (mkFunRefUnqual "unwrap")
                []

    build SM3 (Module _ (SourceFile _ _ _items)) ns =
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndRetChan)
        where
            createTasksAndRetChan (Program chans retChan tasks) =
                Program chans retChan (map create tasks)

            create task@(FullTask _ _ taskE) =
                let initVPE = createVPE : delegateCom task
                    taskE' = createTask $
                             prependToBlock (activateCom task) $
                             BlockExpr [] taskE Nothing noSpan
                    all = prependToBlock initVPE taskE'
                in all <$ task

            createVPE :: Rust.Stmt ()
            createVPE = noSpan <$ [stmt| let mut vpe = VPE::new_child_vpe("test").unwrap(); |]

            activateCom :: FullTask TE.RustTypeAnno a -> [Rust.Stmt ()]
            activateCom (FullTask sends recvs _) =
                map ((flip Semi noSpan . convertExpr SM3) .
                    (\c ->
                        Apply $ Stateful
                            (Apply $ Stateful (Var c) (mkFunRefUnqual "activate") [])
                            (mkFunRefUnqual "unwrap") []))
                    (map (\(SSend (SChan c) _) -> c) sends ++
                     map (\(SRecv _type (SChan c)) -> c) recvs)

            delegateCom :: FullTask TE.RustTypeAnno a -> [Rust.Stmt ()]
            delegateCom (FullTask sends recvs _) =
                map ((flip Semi noSpan . convertExpr SM3) .
                    (\c ->
                        Apply $ Stateful
                            (Apply $ Stateful (Var "vpe") (mkFunRefUnqual "delegate_obj")
                                [Apply $ Stateful (Var c) (mkFunRefUnqual "sel") []])
                            (mkFunRefUnqual "unwrap") []))
                    (map (\(SSend (SChan c) _) -> c) sends ++
                     map (\(SRecv _typ (SChan c)) -> c) recvs)

            createTask :: Rust.Expr () -> Rust.Expr ()
            createTask code =
                let closure =
                        Closure
                            []
                            Value
                            NotAsync
                            Movable
                            (FnDecl [] (Just $ Infer noSpan) False noSpan)
                            code
                            noSpan
                    box =
                        Call
                            []
                            (PathExpr [] Nothing (convertQualBnd (QualifiedBinding (makeThrow ["Box"]) "new")) noSpan)
                            [closure]
                            noSpan
                    run =
                        MethodCall
                            []
                            (convertExpr SM3 $ Var "vpe")
                            (PathSegment (mkIdent "run") Nothing noSpan)
                            [box]
                            noSpan
                in unwrapMC run

    serialize SM3 mod ns = C.serialize mod ns createProgram
        where
            createProgram (Program chans resultExpr tasks) =
                let taskStmts = map (flip Semi noSpan . taskExpression) tasks
                    program = toList chans ++ taskStmts
                in Block (program ++ [NoSemi resultExpr noSpan]) Normal noSpan

unwrapMC inst = MethodCall [] inst (PathSegment (mkIdent "unwrap") Nothing noSpan) [] noSpan


instance Transform (Architectures 'M3)
