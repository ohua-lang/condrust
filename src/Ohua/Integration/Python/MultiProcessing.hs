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

type FullyPyProgram = Program (Py.Statement SrcSpan) (Py.Expr SrcSpan) (Py.Statement SrcSpan) PythonArgType


instance Transform (Architectures 'MultiProcessing)
instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python
    type Chan (Architectures 'MultiProcessing) = Sub.Stmt
    type ATask (Architectures 'MultiProcessing) = Py.Statement SrcSpan

    -- | Convert a backend channel i.e. an arc in the DFG to an expression of the target architecture
    -- | that instantiates the according process communication channel
    convertChannel SMultiProc (SRecv argTy( SChan bnd))=
        let expr = unwrapSubStmt $ convertExpr SMultiProc $ Apply $ Stateless (QualifiedBinding (makeThrow []) "mp.Pipe") []
            send = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_sender"
            recv = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_receiver"
        in Sub.Assign [Sub.Tuple [send, recv]] expr

    -- | Converts an 'incomming edge' of a backend channel into an expression of the target architecture
    --  to receive from a process communication channel 
    -- Todo: Rust wraps that in a 'try'. Receiving is blocking 
    -- and raises EOFError if there's nothing left to receive and the sender is allready closed
        -- > Do I need to wrap this also?
    convertRecv SMultiProc  (SRecv _type (SChan channel)) =
    -- currently this will yield $channel_reciever.recv()           
        convertExpr SMultiProc $
            Apply $ Stateful (TCLang.Var $ channel <> "_receiver") (mkFunRefUnqual "recv") []

    -- | Converts the 'outgoing edge' of a backend channel into an expression of the target architecture
    -- | to send the result of the node computation to a process communication channel 
    -- Todo: Sending is only valid for picklable objects, i.e. basic Types, things def'd at TL of a module
        -- and ADTs thereof. Restriction on the Frontend should actualy prohibit non-TL def's. Also objects 
        -- must not exceed ~ 32MiB. I might need to catch PickleError's or ValueError's here
    convertSend SMultiProc  (SSend (SChan chnlName) toSend)=
        convertExpr SMultiProc $
            Apply $ Stateful (TCLang.Var $ chnlName <> "_sender") (mkFunRefUnqual "send") [TCLang.Var toSend]


    -- Wraps the tasks i.e. codeblocks of host language function calls and 'wiring' to send and
    -- receive into actual independent tasks, in this case named closures because lambdas are to limited in python  
    build SMultiProc (Module fPath (Py.Module stmts)) ns =
        return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)

        where
            -- Tasks are enumerated to create named functions task_1, task_2 ...
            createTasksAndChannels (Program chans retChan tasks)  =
                Program chans retChan (zipWith (curry taskFromSuite) [1..] tasks)

            taskFromSuite:: (Int, FullTask ty Sub.Suite)
                 ->  FullTask ty (Py.Statement SrcSpan)
            taskFromSuite (num, FullTask ins out suite) = FullTask ins out fun
                where fun=
                        Py.Fun
                            (Py.Ident ("task_"++show num) noSpan)
                            []
                            Nothing
                            (subToSuite suite)
                            noSpan

    -- Critical TODO: receiving the result must happen before termination, but is blocking 
    serialize  SMultiProc srcModule ns = return $ callerModule :| algoModules
        where

            convertedAlgoInfos =
                map (\(Algo name expr srcFun) ->
                        (bndToStr name, subToPython expr, Py.fun_args srcFun)) $ ns ^. algos

            algoNames = map (^. algoName) $ ns^.algos
            algoModules = map (makeAlgoModule srcModule) convertedAlgoInfos
            callerModule = makeParallelLib srcModule algoNames

-- | This function generates a new python module for every parallelized function from the input. Statements and imports from
-- | the original code are included in the new file
-- TODO : filter exisitng main functions
makeAlgoModule :: Module -> (String, FullyPyProgram, [Py.Parameter SrcSpan]) -> (FilePath , L.ByteString)
makeAlgoModule srcModule (algoName, prgrm@(Program _ _ tasks), params )=
    let taskList = enumeratedTasks tasks
        newMainFun = buildMain taskList prgrm params
        combinedStmts = combineStatements srcModule (prgrm, newMainFun)
        modName = algoName <> ".py"
        printableCode = encodePretty combinedStmts
    in (modName, printableCode)


-- | This function produces a parallelized version of the input module that 
-- |    a) imports the modules generated from each function declaration
-- |    b) calls the respective main functions of those modules, instead of the original function code
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
            in [Py.StmtExpr
                (Py.Call calledFun args noSpan )
                noSpan]

-- | Gererate an import statement for a given algo binding
makeImport :: Binding -> Py.Statement SrcSpan
makeImport algoBnd =
    let modName = bndToStr algoBnd
        alias = modName <> "_parallel"
    -- from . import algo as algo_parallel
    in Py.FromImport
        (Py.ImportRelative 1 Nothing noSpan) 
        (Py.FromItems [Py.FromItem  (mkIdent modName)  
                                    (Just (mkIdent alias))
                                    noSpan
                      ] noSpan)
        noSpan

combineStatements ::
    Module
    -> (FullyPyProgram , Py.Statement SrcSpan)
    -> Py.Module SrcSpan
combineStatements srcModule (Program channelInits _ nodeFuns, multiMain) = Py.Module combinedStatements
    where
        (Module path (Py.Module originalStmts)) = srcModule
        taskFunDefs = map taskExpression nodeFuns
        combinedStatements = importMPStmt
                             : toList channelInits
                             ++ taskFunDefs
                             -- ToDo: I should separate import statements from the rest 
                             -- insert them before the other 'originalStmts'
                             ++ originalStmts
                             ++ [multiMain]

-- | Algo modules need a main functin oto be called by the replaced function. This main contains statements to
-- | 0) declare the parameters of the replaced function global 
-- | i) initialize a tasks and a process list
-- | ii) assign a process to each task 
-- | iii) start processes 
-- | iv) receive the result
-- | v) close and cleanup process ressources
-- | vi) return the overall result

buildMain:: [String] -> FullyPyProgram -> [Py.Parameter SrcSpan]-> Py.Statement SrcSpan
buildMain taskNames (Program chnls resExpr tasks) params =  Py.Fun (mkIdent "main") params' resAnno mainBlock noSpan
        where
            taskList = Py.List (map (toPyVar . mkIdent) taskNames) noSpan
            initTasksStmt = Py.Assign [(toPyVar .mkIdent) "tasks"] taskList noSpan
            assignRes = Py.Assign [toPyVar . mkIdent $ "result"] resExpr noSpan
            mainBasic = [
                initTasksStmt,
                initProcs, assignTasks,
                startProcs, assignRes,
                terminateProcs, joinProcs,
                returnResult]
            (params', glob_decls) = paramsAndGlobals params
            mainBlock = glob_decls ++ mainBasic
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

modName (Py.Param (Py.Ident name _) mTyp mDef anno) modV = Py.Ident (name++modV) noSpan


subToPython :: Program Stmt Stmt (Py.Statement SrcSpan) PythonArgType
    -> FullyPyProgram
subToPython (Program c r t ) =  Program (map subToStmt c) (subToExpr . unwrapSubStmt $ r) t

enumeratedTasks :: [FullTask PythonArgType (Py.Statement SrcSpan)] -> [String]
enumeratedTasks  tasks =  zipWith (\ task i -> "task_" ++ show i) tasks [1..]

encodePretty :: Py.Module SrcSpan -> L.ByteString
encodePretty = encodeUtf8 . prettyText
