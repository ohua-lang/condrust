{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ohua.Integration.Rust.Architecture.M3 where

import Language.Rust.Data.Ident (mkIdent)
import Language.Rust.Quote
import Language.Rust.Parser (parse', inputStreamFromString)
import qualified Language.Rust.Syntax as Rust hiding (Rust)
import Ohua.Backend.Lang (Com (..), ComType(..), Channel)
import Ohua.Backend.Types hiding (Expr, convertExpr)
import Ohua.Integration.Architecture
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Rust.Architecture.Common as C
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Backend.Convert
  ( convertBlock,
    convertCallRef,
    convertExp,
    convertIdentPat,
    convertPat,
    convertStmt,
    convertQualBnd,
    noSpan,
    prependToBlock,
  )
import qualified Ohua.Integration.Rust.Backend.Subset as Sub
import qualified Ohua.Integration.Rust.TypeExtraction as TE
import qualified Ohua.Integration.Rust.Types as RT
import Ohua.Prelude
import Ohua.Integration.Rust.Backend.Passes (propagateMut)

convertChan :: Sub.BindingMode -> Channel TE.RustTypeAnno -> Rust.Stmt ()
convertChan rxMutability (SRecv _ty (SChan bnd)) =
    let channel = noSpan <$ [expr| channel() |]
     in Rust.Local
          ( convertPat $
              Sub.TupP
                [ Sub.IdentPat Sub.Immutable $ bnd <> "_tx",
                  Sub.IdentPat rxMutability $ bnd <> "_rx"
                ]
          )
          Nothing
          (Just channel)
          []
          noSpan

instance Architecture (Architectures 'M3) where
  type Lang (Architectures 'M3) = Language 'Rust
  type Chan (Architectures 'M3) = Rust.Stmt ()
  type ATask (Architectures 'M3) = Rust.Expr ()

  convertChannel SM3 = convertChan Sub.Immutable 
  convertRetChannel SM3 = convertChan Sub.Mutable

  -- QUESTION: This 'invariant' is probably imposed by M3 requiring 'turbo fish'
  -- typing for the recv. calls i.e. a_0_0_rx.recv_msg::<String,>().unwrap() so if any return
    -- type is unknown to Ohua, this will blow up
    -- &&convertRecv SM3 (SRecv TypeVar (SChan channel)) = (trace $ show channel) error "Invariant broken!"
  -- ISSUE: To make progress, I'll insert a paceholder type annotation. The real type needs to be derived earlier!!
  convertRecv SM3 (SRecv TypeVar (SChan channel)) = 
      error $ "A TypeVar was introduced for channel" <>  show channel <> ". This is a compiler error, please report (fix if you are me :-))"
    
  -- ToDo: Refactor when case handling is clear to get rid of duplicate code  
  -- QUESTION: Can we use the same pattern here as for SM? I'll just use it for now to keep on working with the test code. 
  convertRecv SM3 (SRecv (Type typ) (SChan channel)) =
    let ty' = case typ of 
                (TE.Self ty _ _mut) -> noSpan <$ ty
                -- error "Not yet implemented: Recv of reference type"
                (TE.Normal ty) -> noSpan <$ ty
                  -- ISSUE: Find correct reaction here. For now I'll pretend channel.recv::<(ty1, ty2 ...)>().unwrap() is ok
                -- _ -> toRustTy $ Type typ
        rcv =
          Sub.MethodCall
            (Sub.Var $ channel <> "_rx0")
            (Sub.CallRef (asQualBind "recv") $ Just $ Sub.AngleBracketed [Sub.TypeArg $ Sub.RustType ty'])
            []
    in Sub.Try rcv

  -- QUESTION: Why is sending a binding (Right binding) undefined and (Left Lit ty) is ok?
  -- REMINDER: To make progress in testing stuff I'll adopt the handling from SM here. Check if
    -- thats valid and replace otherwise 
  convertSend SM3 (SSend (SChan channel) toSend) = case toSend of
    Right num@NumericLit{} -> asMethodCall $ Sub.Lit num
    Right b@BoolLit{} -> asMethodCall $ Sub.Lit b
    Right s@StringLit{} -> asMethodCall $ Sub.Lit s
    Right UnitLit -> asMethodCall $ Sub.Lit UnitLit
    Right (EnvRefLit bnd) -> asMethodCall $ Sub.Var bnd
    Right (FunRefLit _) -> error "Invariant broken: Got asked to send a function reference via channel which should have been caught in the backend."
    (Left d) -> asMethodCall $ Sub.Var d
    where 
      asMethodCall item = 
        Sub.Try
              (Sub.MethodCall (Sub.Var $ channel <> "_tx") (Sub.CallRef (asQualBind "send") Nothing) [item])

  build SM3 (RT.Module _ (Rust.SourceFile _ _ _items)) ns =
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndRetChan)
    where
      createTasksAndRetChan (Program chans retChan tasks) =
        Program chans retChan $ map createActivity tasks

      createActivity task@(FullTask sends recvs _) = FullTask sends recvs $ createActivity' task

      createActivity' (FullTask sends recvs taskE) =
        let
          extractSend :: Com 'Send t -> Binding 
          extractSend (SSend (SChan c) _) = c
          extractRecv :: Com 'Recv t -> Binding
          extractRecv (SRecv _ (SChan c)) = c
          closureParams vars ty extract =
            map (\v ->
                  Rust.Arg
                    [] 
                    (Just $ convertIdentPat $ Sub.IdentPat Sub.Immutable $ extract v) 
                    (Rust.PathTy Nothing (convertQualBnd ty) noSpan)
                    noSpan)
                vars
          cParams = (closureParams sends "Sender" extractSend) <> 
                    (closureParams recvs "Receiver" extractRecv)
          closureArgs vars extract =
            map (convertExp . Sub.Var  . extract) vars
          cArgs = (closureArgs sends extractSend) <> 
                  (closureArgs recvs extractRecv)
          taskCode = Rust.BlockExpr [] (convertBlock taskE) Nothing noSpan
          taskClosure =
            Rust.Closure
              []
              Rust.Value
              Rust.NotAsync
              Rust.Immovable
              (Rust.FnDecl cParams Nothing False noSpan)
              taskCode
              noSpan
          taskCall = Rust.Call [] taskClosure cArgs noSpan
          exprToTokenStream = parse' @Rust.TokenStream . inputStreamFromString . C.renderStr
        in 
          Rust.MacExpr 
            [] 
            (Rust.Mac (convertQualBnd "activity") (exprToTokenStream taskCall) noSpan) 
            noSpan


  -- REMINDER: Replace Placeholder
  serialize SM3 mod placeholder ns = C.serialize mod ns createProgram placeholder
    where
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

instance Transform (Architectures 'M3) where
  -- transformTaskExpr = id
  transformTask _ _ = propagateMut


