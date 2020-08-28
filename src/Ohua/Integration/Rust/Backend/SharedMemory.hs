module Ohua.Integration.Rust.Backend.SharedMemory where

import Ohua.Prelude

import Ohua.Integration.Rust.Backend


newtype SharedMem = SharedMem RustLang

instance Architecture SharedMem where 
    type Task SharedMem = Expr ()
    type Channel SharedMem = Stmt ()

    build ns (SharedMem (Module (_, SourceFile _ _ items))) = 
        return ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (TCProgram chans retChan tasks) = 
                TCProgram
                    ((createChannels chans) ++ [(convertChannel retChan)])
                    retChan
                    (map createTask tasks)

            createTask :: Block () -> Expr ()
            createTask code = 
                Clojure
                    []
                    Movable
                    Value
                    (FnDecl [] (Just Infer ()) false ())
                    code
                    ()

            createChannels :: [Channel] -> [Stmt ()]
            createChannels = map convertChannel

    serialize ns (SharedMem (Module (path, SourceFile modName atts items))) =
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
                                [task]
                    push t =
                        Apply $
                            Stateful
                                "tasks"
                                (QualifiedBinding (makeThrow []) "push")
                                [t]
                    taskStmts = map (flip Semi noSpan . convertExpr . push . box) tasks
                    taskRunStmt = () <$ [stmt| run(tasks); |]
                    resultExpr = convertExpr $ Receive 0 retChan
                    program = chans ++ retChan ++ [taskInitStmt] ++ taskStmts ++ [taskRunStmt]
                in appendToBlock program resultExpr

instance ConvertChannel (Stmt ()) where
    convertChannel (Channel bnd numCopies) = 
        let stmt = Apply $ 
                    Stateless 
                        (QualifiedBinding (makeThrow ["ohua", "arcs", "Channel"]) "new") 
                        [TCLang.Lit $ NumericLit $ fromIntegral numCopies]
        in Local 
                (mkSimpleBinding $ unwrap bnd)
                Nothing
                (Just $ convertExpr stmt)
                []
                noSpan
