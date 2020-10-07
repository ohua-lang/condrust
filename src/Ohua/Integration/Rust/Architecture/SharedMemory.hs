{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Architecture.SharedMemory where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Convert
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Types as RT
import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Quote
import Language.Rust.Pretty ( pretty' )
import Language.Rust.Data.Ident
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.HashMap.Lazy as HM
import System.FilePath (takeFileName)


build (Module (_, SourceFile _ _ items)) ns = 
    return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
    where
        createTasksAndChannels (TCProgram chans retChan@(SRecv chan) tasks) = 
            TCProgram
                (createChannels chans ++ createChannels [chan])
                retChan
                (map createTask tasks)

        createTask :: Block () -> Expr ()
        createTask code = 
            Closure
                []
                Movable
                Value
                (FnDecl [] (Just $ Infer noSpan) False noSpan)
                (BlockExpr [] code noSpan)
                noSpan

        createChannels :: [Channel] -> [Stmt ()]
        createChannels = map convertChannel

serialize (Module (path, SourceFile modName atts items)) ns =
    let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) $ ns^.algos
        src    = SourceFile modName atts $ map (replaceAlgo algos') items
        render = encodeUtf8 . (<> "\n") . renderLazy . layoutSmart defaultLayoutOptions . pretty'
        path' = takeFileName path -- TODO verify this!
    in return $ (path', render src) :| []
    where
        replaceAlgo algos = \case
                f@(Fn atts vis ident decl@(FnDecl _args _ _ _) s c abi gen _ span) ->
                    case HM.lookup (toBinding ident) algos of
                        Just algo -> 
                            Fn atts vis ident decl s c abi gen (span <$ createProgram algo) span
                        Nothing -> f
                i -> i
        
        createProgram (TCProgram chans retChan tasks) =
            let taskInitStmt = noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
                box task =
                    Call 
                        []
                        (PathExpr [] Nothing (convertQualBnd (QualifiedBinding (makeThrow ["Box"]) "new")) noSpan)
                        [task]
                        noSpan
                push t =
                    MethodCall
                        []
                        (convertExpr $ Var "tasks")
                        (mkIdent "push")
                        Nothing
                        [t]
                        noSpan
                taskStmts = map (flip Semi noSpan . push . box) tasks
                taskRunStmt = () <$ [stmt| run(tasks); |]
                resultExpr = convertExpr $ ReceiveData retChan
                program = chans ++ [taskInitStmt] ++ taskStmts ++ [taskRunStmt]
            in Block (program ++ [NoSemi resultExpr noSpan]) Normal noSpan

instance ConvertChannel (Stmt ()) where
    convertChannel (SChan bnd) = 
        let stmt = Apply $ 
                    Stateless 
                        (QualifiedBinding (makeThrow ["std","sync","mpsc"]) "channel") 
                        []
        in Local
                (TupleP [mkSimpleBinding $ bnd <> "_tx", mkSimpleBinding $ bnd <> "_rx"] Nothing noSpan)
                Nothing
                (Just $ convertExpr stmt)
                []
                noSpan
