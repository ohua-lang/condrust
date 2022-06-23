{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Prelude


import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Python.NewBackend
import Ohua.Integration.Python.Backend.Subset as Sub
import Ohua.Integration.Python.Backend.Convert
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.Types
import Ohua.Integration.Python.TypeExtraction


import qualified Language.Python.Common.AST as Py
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Common.Pretty (prettyText)

import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import System.FilePath (takeFileName)
import Language.Python.Common (ImportRelative(import_relative_dots))
import Data.List (nub)

type FullyPyProgram = Program (Py.Statement SrcSpan) (Py.Expr SrcSpan) (Py.Statement SrcSpan) PythonArgType


-- instance Transform (Architectures 'MultiProcessing)
instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python
    type Chan (Architectures 'MultiProcessing) = Sub.Stmt
    type ATask (Architectures 'MultiProcessing) = Py.Statement SrcSpan

    {- | Convert a backend channel i.e. an arc in the DFG to an expression of the target architecture
         that instantiates the according process communication channel
    -}
    convertChannel SMultiProc (SRecv argTy( SChan bnd))=
        let expr = unwrapSubStmt $ convertExpr SMultiProc $ Apply $ Stateless (QualifiedBinding (makeThrow []) "mp.Pipe") []
            send = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_sender"
            recv = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_receiver"
        in Sub.Assign [Sub.Tuple [send, recv]] expr

    {- | Converts an 'incomming edge' of a backend channel into an expression of the target architecture
      to receive from a process communication channel 
    -}
    -- Todo: Rust wraps that in a 'try'. Receiving is blocking 
    -- and raises EOFError if there's nothing left to receive and the sender is allready closed
        -- > Do I need to wrap this also?
    convertRecv SMultiProc  (SRecv _type (SChan channel)) =
     -- currently this will yield $channel_reciever.recv()           
        convertExpr SMultiProc $
            Apply $ Stateful (TCLang.Var $ channel <> "_receiver") (toQualBinding "recv") []

    {- | Converts the 'outgoing edge' of a backend channel into an expression of the target architecture
         to send the result of the node computation to a process communication channel 
    -}
    -- Todo: Sending is only valid for picklable objects, i.e. basic Types, things def'd at TL of a module
        -- and ADTs thereof. Restriction on the Frontend should actualy prohibit non-TL def's. Also objects 
        -- must not exceed ~ 32MiB. I might need to catch PickleError's or ValueError's here
    convertSend SMultiProc  (SSend (SChan chnlName) toSend) =
        let sendItem = case toSend of
                Left varBnd -> TCLang.Var varBnd
                Right literal -> TCLang.Lit literal
        in convertExpr SMultiProc $
            Apply $ Stateful (TCLang.Var $ chnlName <> "_sender") (toQualBinding "send") [sendItem]


    {- | Wraps the tasks i.e. codeblocks of host language function calls and 'wiring' to send and
         receive into actual independent tasks, in this case named closures because lambdas are to limited in python  
    -}
    build SMultiProc (Module fPath (Py.Module stmts)) ns =
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            -- ^ Tasks are enumerated to create named functions task_1, task_2 ...
            createTasksAndChannels (Program chans retChan tasks)  =
                Program chans retChan (zipWith (curry taskFromSuite) [1..] tasks)


            taskFromSuite:: (Int, FullTask ty Sub.Suite)
                 ->  FullTask ty (Py.Statement SrcSpan)
            taskFromSuite (num, FullTask ins out suite) = FullTask ins out fun
                where
                    fun=
                        Py.Fun
                            (Py.Ident ("task_"++show num) noSpan)
                            (nub (inputChnls ++ outChnl))
                            Nothing
                            (subToSuite suite)
                            noSpan

                    inputChnls = map chnlToParameter ins
                    outChnl = map chnlToParameter out


    {- | Build the process management arround the tasks for each algorith i.e. produce algoModules,
         and build a new caller module to replace the input libraray that i) imports those algo algoModules
         as 'algo_parallel' and b) contains all original function calls with function bodies replaced by a 
         call to the respective parallel module         
    -}
    serialize  SMultiProc srcModule placeholder ns  = return $ callerModule:| ([lib_from_frontend] ++ algoModules)
        where

            convertedAlgoInfos =
                map (\(Algo name expr srcFun) ->
                        (bndToStr name, subToPython expr, Py.fun_args srcFun)) $ ns ^. algos

            algoNames = map (^. algoName) $ ns^.algos
            algoModules = map (makeAlgoModule srcModule) convertedAlgoInfos
            callerModule = makeParallelLib srcModule algoNames
            lib_from_frontend = ("placeholderlib.py", "# Hier koennnte Ihre Werbung stehen")

chnlToParameter :: Com comTy argTy  -> Py.Parameter SrcSpan
chnlToParameter chnl = Py.Param (chnlToIdent chnl) Nothing Nothing noSpan

chnlToVar :: Com comTy argTy  -> Py.Expr SrcSpan
chnlToVar chnl = Py.Var (chnlToIdent chnl) noSpan

chnlToIdent :: Com comTy argTy  -> Py.Ident SrcSpan
chnlToIdent (SRecv t (SChan chnlName)) = fromBinding (chnlName <> "_receiver")
chnlToIdent (SSend (SChan chnlName) toSend) = fromBinding (chnlName <> "_sender")

{- | This function generates a new python module for every parallelized function from the input. 
     Statements and imports from the original code are included in the new file
-}
makeAlgoModule :: Module -> (String, FullyPyProgram, [Py.Parameter SrcSpan]) -> (FilePath , L.ByteString)
makeAlgoModule (Module path (Py.Module inputCode)) (algoName, prgrm@(Program _ _ tasks), params) =
    let taskList = enumeratedTasks tasks
        chnlsPerTask = map channelsFromTask tasks
        newMainFun = buildMain taskList chnlsPerTask prgrm params
        originalScope = scopeOfAlgo algoName inputCode
        combinedStmts = combineStatements originalScope prgrm newMainFun
        modName = algoName <> ".py"
        printableCode = encodePretty combinedStmts
    in (modName, printableCode)



scopeOfAlgo :: String -> [Py.Statement SrcSpan] -> [Py.Statement SrcSpan]
scopeOfAlgo aName [] = []
-- we ignore algos as they are spliced into our taks graph
scopeOfAlgo aName (Py.Fun{} : stmts) = scopeOfAlgo aName stmts
scopeOfAlgo aName (stmt: stmts) =  stmt : scopeOfAlgo aName stmts



-- | Task functions are called with a list of (unique -> nub) channels they use as arguments.
-- | Here we extract them.
channelsFromTask :: FullTask PythonArgType (Py.Statement SrcSpan) -> [Py.Expr SrcSpan]
channelsFromTask (FullTask ins outs fun) = nub (map chnlToVar ins ++ map chnlToVar outs)

{- | This function produces a parallelized version of the input module that 
        a) imports the modules generated from each function declaration
        b) calls the respective main functions of those modules, instead of the original function code
-}
makeParallelLib :: Module -> [Binding] -> (FilePath , L.ByteString)
makeParallelLib (Module path (Py.Module statements)) algoNames =
    let newImports = map makeImport algoNames
        replacedStmts = map replaceFunCode statements
        path' = takeFileName path
        printableCode = encodePretty (Py.Module (newImports ++ replacedStmts))
    in ( path', printableCode)

-- | Replaces the code of every declared function by a call to it's parallelized code in the repective imported module
replaceFunCode:: Py.Statement SrcSpan -> Py.Statement SrcSpan
replaceFunCode stmt = case stmt of
    Py.Fun id params mRType code annot -> Py.Fun id params mRType (replaceCode id params) noSpan
    anyOther -> anyOther
    where
        replaceCode :: Py.Ident SrcSpan -> [Py.Parameter SrcSpan] -> Py.Suite SrcSpan
        replaceCode (Py.Ident name _) params =
            let funId = name <> "_parallel.main"
                calledFun =  Py.Var (mkIdent funId) noSpan
                args = map paramToArg params
            in [Py.Return
                (Just (Py.Call calledFun args noSpan ))
                noSpan]

-- | Gererate an import statement for a given algo binding
makeImport :: Binding -> Py.Statement SrcSpan
makeImport algoBnd =
    let modName = bndToStr algoBnd
        alias = modName <> "_parallel"
    in Py.Import [
            Py.ImportItem [mkIdent modName]  -- import algo
            (Just (mkIdent alias))              -- as algo_parallel
            noSpan]
        noSpan
    -- TODO: Check best import option, trelative imports only work for defined packages
    -- Question: Shall lib and algo modules form a package
    -- from . import algo as algo_parallel
    {-in Py.FromImport
        (Py.ImportRelative 1 Nothing noSpan) 
        (Py.FromItems [Py.FromItem  (mkIdent modName)  
                                    (Just (mkIdent alias))
                                    noSpan
                      ] noSpan)
        noSpan -}

combineStatements ::
    [Py.Statement SrcSpan]
    -> FullyPyProgram
    -> Py.Statement SrcSpan
    -> Py.Module SrcSpan
combineStatements originalStmts (Program channelInits _ nodeFuns) multiMain = Py.Module combinedStatements
    where
        taskFunDefs = map taskExpression nodeFuns
        combinedStatements = importMPStmt
                             -- ToDo: I should separate import statements from the rest 
                             -- insert them before the other 'originalStmts'
                             : originalStmts
                             ++ taskFunDefs
                             ++ [multiMain]

-- | Algo modules need a main function to be called by the replaced function. This main contains statements to
-- | 0) declare the parameters of the replaced function global 
-- | i) initialize a tasks and a process list
-- | ii) intialize the comunication channels 
-- | iii) assign a process to each task using channels as arguments
-- | iv) start processes 
-- | v) receive the result
-- | vi) close and cleanup process ressources
-- | vii) return the overall result

buildMain:: [String]-> [[Py.Expr SrcSpan]] -> FullyPyProgram -> [Py.Parameter SrcSpan]-> Py.Statement SrcSpan
buildMain taskNames chnlsPerTask (Program chnls resExpr tasks) params =  Py.Fun (mkIdent "main") params' resAnno mainBlock noSpan
        where
            taskList = Py.List (map (toPyVar . mkIdent) taskNames) noSpan
            initTasksStmt = Py.Assign [(toPyVar .mkIdent) "tasks"] taskList noSpan
            chnlsList = Py.List (map (`Py.List` noSpan) chnlsPerTask) noSpan 
            initChnlsList = Py.Assign [(toPyVar .mkIdent) "channels"] chnlsList noSpan
            assignRes = Py.Assign [toPyVar . mkIdent $ "result"] resExpr noSpan
            mainBasic = [
                initTasksStmt,
                initChnlsList,
                initProcs, assignTasks,
                startProcs, assignRes,
                terminateProcs, joinProcs,
                returnResult]
            (params', glob_decls) = paramsAndGlobals params
            mainBlock = glob_decls ++ toList chnls ++ mainBasic
            resAnno = Nothing

-- | A call to the main of an algo module must contain the original parameters of the algo. In the main, those 
-- | parameters must be declared global to be used by all generated  tasks. This function generates the according python statements.
paramsAndGlobals:: [Py.Parameter SrcSpan] ->  ([Py.Parameter SrcSpan], [Py.Statement SrcSpan])
paramsAndGlobals [] = ([],[])
paramsAndGlobals params = (renamed, [globalStmt, assign])
    where
        originalIDs = map Py.param_name params
        helperIDs = map (`modName` "_1") params
        globalStmt = Py.Global originalIDs noSpan
        varTupleLeft = Py.Tuple (map (`Py.Var` noSpan) originalIDs) noSpan
        varTupleRight = Py.Tuple (map (`Py.Var` noSpan) helperIDs) noSpan
        assign = Py.Assign [varTupleLeft] varTupleRight noSpan
        renamed = zipWith (curry replaceId) params helperIDs


paramToArg:: Py.ParameterSpan -> Py.ArgumentSpan
paramToArg (Py.Param id mTyp mDef anno) = Py.ArgExpr (Py.Var id noSpan) noSpan
paramToArg _ = error "unsupported parameter type. this should have been caught in the frontend"

replaceId:: (Py.ParameterSpan, Py.IdentSpan) -> Py.ParameterSpan
replaceId (Py.Param id mTyp mDef anno, newId) = Py.Param newId mTyp mDef anno


modName :: Py.Parameter annot -> [Char] -> Py.Ident SrcSpan
modName (Py.Param (Py.Ident name _) mTyp mDef anno) modV = Py.Ident (name++modV) noSpan


subToPython :: Program Stmt Stmt (Py.Statement SrcSpan) PythonArgType -> FullyPyProgram
subToPython (Program c r t ) =  Program (map subToStmt c) (subToExpr . unwrapSubStmt $ r) t

enumeratedTasks :: [FullTask PythonArgType (Py.Statement SrcSpan)] -> [String]
enumeratedTasks  tasks =  zipWith (\ task i -> "task_" ++ show i) tasks [1..]

