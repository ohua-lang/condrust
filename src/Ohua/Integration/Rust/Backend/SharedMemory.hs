{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Backend.SharedMemory where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Convert
import Ohua.Integration.Rust.Backend
import Ohua.Integration.Rust.Types
import Ohua.Integration.Rust.Util

import Language.Rust.Syntax as Rust hiding (Rust)
import Language.Rust.Quote
import Language.Rust.Pretty ( pretty' )
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.HashMap.Lazy as HM
import System.FilePath (takeFileName)


newtype SharedMem = SharedMem RustLang

instance Architecture SharedMem where 
    type Integ SharedMem = RustLang
    type Task SharedMem = Expr ()
    type Chan SharedMem = Stmt ()

    build (SharedMem (Module (_, SourceFile _ _ items))) ns = 
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (TCProgram chans retChan tasks) = 
                TCProgram
                    (createChannels chans ++ [convertChannel retChan])
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

    serialize (SharedMem (Module (path, SourceFile modName atts items))) ns =
        let algos' = HM.fromList $ map (\(Algo name expr) -> (name, expr)) $ ns^.algos
            src    = SourceFile modName atts $ map (replaceAlgo algos') items
            render = encodeUtf8 . (<> "\n") . renderLazy . layoutSmart defaultLayoutOptions . pretty'
            path' = takeFileName path -- TODO verify this!
        in return $ (path', render src) :| []
        where
            replaceAlgo algos = \case
                    f@(Fn atts vis ident decl@(FnDecl args _ _ _) s c abi gen _ span) ->
                        case HM.lookup (toBinding ident) algos of
                            Just algo -> 
                                Fn atts vis ident decl s c abi gen (span <$ createProgram algo) span
                            Nothing -> f
                    i -> i
            
            createProgram (TCProgram chans (Channel retChan _) tasks) =
                let taskInitStmt = noSpan <$ [stmt| let mut tasks:Vec<Box<dyn FnOnce() -> Result<(), RunError>+ Send >> = Vec::new(); |]
                    box task =
                        Apply $
                            Stateless
                                (QualifiedBinding (makeThrow ["Box"]) "new") 
                                [task] -- FIXME This does not work!
                    push t =
                        Apply $
                            Stateful
                                "tasks"
                                (QualifiedBinding (makeThrow []) "push")
                                [t]
                    taskStmts = map (flip Semi noSpan . convertExpr . push . box) tasks
                    taskRunStmt = () <$ [stmt| run(tasks); |]
                    resultExpr = convertExpr $ Receive 0 retChan
                    program = chans ++ [taskInitStmt] ++ taskStmts ++ [taskRunStmt]
                in prependToBlock program resultExpr

instance ConvertChannel (Stmt ()) where
    convertChannel (Channel bnd numCopies) = 
        let stmt = Apply $ 
                    Stateless 
                        (QualifiedBinding (makeThrow ["ohua", "arcs", "Channel"]) "new") 
                        [TCLang.Lit $ NumericLit $ fromIntegral numCopies]
        in Local 
                (mkSimpleBinding bnd)
                Nothing
                (Just $ convertExpr stmt)
                []
                noSpan
