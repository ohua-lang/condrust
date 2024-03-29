{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Commons.Prelude


import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Python.Backend
import Ohua.Integration.Python.Backend.Subset as Sub
import Ohua.Integration.Python.Backend.Convert
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeHandling


import qualified Language.Python.Common.AST as Py
import Language.Python.Common.SrcLocation (SrcSpan)
-- import Language.Python.Common.Pretty (prettyText)

import qualified Data.ByteString.Lazy as L
-- import qualified Data.HashMap.Lazy as HM
-- import qualified Data.List.NonEmpty as NE

import System.FilePath (takeFileName)
-- import Language.Python.Common (ImportRelative(import_relative_dots))
import Data.List (nub)

-- data Program chan retChan expr embExpr ty 
type FullyPyProgram = Program (Py.Statement SrcSpan) (Py.Expr SrcSpan) (Py.Statement SrcSpan) (Py.Expr SrcSpan) PythonVarType


-- instance Transform (Architectures 'MultiProcessing)
instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python
    type Chan (Architectures 'MultiProcessing) = Sub.Stmt
    type ATask (Architectures 'MultiProcessing) = Py.Statement SrcSpan

    {- | Convert a backend channel i.e. an arc in the DFG to an expression of the target architecture
         that instantiates the according process communication channel
    -}
    convertChannel a@SMultiProc{} (SRecv _argTy( SChan bnd))=
        let expr = unwrapSubStmt $ convertExpr a $ Apply $ Stateless (QualifiedBinding (makeThrow []) "mp.Pipe") []
            send = unwrapSubStmt $ convertExpr a $ TCLang.Var $ bnd <> "_sender"
            recv = unwrapSubStmt $ convertExpr a $ TCLang.Var $ bnd <> "_receiver"
        in Sub.Assign [Sub.Tuple [send, recv]] expr

    {- | Converts an 'incomming edge' of a backend channel into an expression of the target architecture
      to receive from a process communication channel 
    -}
    -- Todo: Rust wraps that in a 'try'. Receiving is blocking 
    -- and raises EOFError if there's nothing left to receive and the sender is allready closed
        -- > Do I need to wrap this also?
    convertRecv a@SMultiProc{}  (SRecv _type (SChan channel)) =
     -- currently this will yield $channel_reciever.recv()           
        convertExpr a $
            Apply $ Stateful (TCLang.Var $ channel <> "_receiver") (toQualBinding "recv") []

    {- | Converts the 'outgoing edge' of a backend channel into an expression of the target architecture
         to send the result of the node computation to a process communication channel 
    -}
    -- Todo: Sending is only valid for picklable objects, i.e. basic Types, things def'd at TL of a module
        -- and ADTs thereof. Restriction on the Frontend should actualy prohibit non-TL def's. Also objects 
        -- must not exceed ~ 32MiB. I might need to catch PickleError's or ValueError's here
    convertSend a@SMultiProc{}  (SSend (SChan chnlName) toSend) =
        let sendItem = case toSend of
                Left varBnd -> TCLang.Var varBnd
                Right literal -> TCLang.Lit literal
        in convertExpr a $
            Apply $ Stateful (TCLang.Var $ chnlName <> "_sender") (toQualBinding "send") [sendItem]


    {- | Wraps the tasks i.e. codeblocks of host language function calls and 'wiring' to send and
         receive into actual independent tasks, in this case named closures because lambdas are to limited in python  
    -}
    build SMultiProc{} (Module _fPath (Py.Module _stmts)) modNS =
        return $ modNS & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            -- ^ Tasks are enumerated to create named functions task_1, task_2 ...
            createTasksAndChannels (Program chans retChan tasks)  =
                Program chans retChan (zipWith (curry taskFromSuite) [1..] tasks)


            taskFromSuite:: (Int, FullTask (Py.Expr SrcSpan) ty Sub.Suite)
                 ->  FullTask (Py.Expr SrcSpan) ty (Py.Statement SrcSpan)
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
    serialize SMultiProc{} srcModule placeholder modNS  = return $ callerModule:| (lib_from_frontend : algoModules)
        where

            convertedAlgoInfos =
                map (\(Algo aName _aType expr srcFun) ->
                        (bndToStr aName, subToPython expr, Py.fun_args srcFun)) $ modNS ^. algos

            algoNames = map (^. algoName) $ modNS^.algos
            algoModules = map (makeAlgoModule srcModule) convertedAlgoInfos
            callerModule = makeParallelLib srcModule algoNames
            (Module libname lib) = placeholder
            lib_from_frontend = (libname, encodePretty lib)

chnlToParameter :: Com comTy embExpr argTy  -> Py.Parameter SrcSpan
chnlToParameter chnl = Py.Param (chnlToIdent chnl) Nothing Nothing noSpan

chnlToVar :: Com comTy embExpr argTy  -> Py.Expr SrcSpan
chnlToVar chnl = Py.Var (chnlToIdent chnl) noSpan

chnlToIdent :: Com comTy embExpr argTy  -> Py.Ident SrcSpan
chnlToIdent (SRecv _ty (SChan chnlName)) = fromBinding (chnlName <> "_receiver")
chnlToIdent (SSend (SChan chnlName) _toSend) = fromBinding (chnlName <> "_sender")

{- | This function generates a new python module for every parallelized function from the input. 
     Statements and imports from the original code are included in the new file
-}
makeAlgoModule :: Module -> (String, FullyPyProgram, [Py.Parameter SrcSpan]) -> (FilePath , L.ByteString)
makeAlgoModule (Module _fpath (Py.Module inputCode)) (algName, prgrm@(Program _ _ tasks), params) =
    let taskList = enumeratedTasks tasks
        chnlsPerTask = map channelsFromTask tasks
        newMainFun = buildMain taskList chnlsPerTask prgrm params
        originalScope = scopeOfAlgo algName inputCode
        combinedStmts = combineStatements originalScope prgrm newMainFun
        pyModName = algName <> ".py"
        printableCode = encodePretty combinedStmts
    in (pyModName, printableCode)



scopeOfAlgo :: String -> [Py.Statement SrcSpan] -> [Py.Statement SrcSpan]
scopeOfAlgo _aName [] = []
-- we ignore algos as they are spliced into our taks graph
scopeOfAlgo aName (Py.Fun{} : stmts) = scopeOfAlgo aName stmts
scopeOfAlgo aName (stmt: stmts) =  stmt : scopeOfAlgo aName stmts



-- | Task functions are called with a list of (unique -> nub) channels they use as arguments.
-- | Here we extract them.
channelsFromTask :: FullTask (Py.Expr SrcSpan) PythonVarType (Py.Statement SrcSpan) -> [Py.Expr SrcSpan]
channelsFromTask (FullTask ins outs _fun) = nub (map chnlToVar ins ++ map chnlToVar outs)

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
    Py.Fun fId params mRType _code _annot -> Py.Fun fId params mRType (replaceCode fId params) noSpan
    anyOther -> anyOther
    where
        replaceCode :: Py.Ident SrcSpan -> [Py.Parameter SrcSpan] -> Py.Suite SrcSpan
        replaceCode (Py.Ident iName _) params =
            let funId = iName <> "_parallel.main"
                calledFun =  Py.Var (mkIdent funId) noSpan
                args = map paramToArg params
            in [Py.Return
                (Just (Py.Call calledFun args noSpan ))
                noSpan]

-- | Gererate an import statement for a given algo binding
makeImport :: Binding -> Py.Statement SrcSpan
makeImport algoBnd =
    let pyModName = bndToStr algoBnd
        pyalias = pyModName <> "_parallel"
    in Py.Import [
            Py.ImportItem [mkIdent pyModName]  -- import algo
            (Just (mkIdent pyalias))              -- as algo_parallel
            noSpan]
        noSpan
    -- TODO: Check best import option, trelative imports only work for defined packages
    -- Question: Shall lib and algo modules form a package
    -- from . import algo as algo_parallel
    {-in Py.FromImport
        (Py.ImportRelative 1 Nothing noSpan) 
        (Py.FromItems [Py.FromItem  (mkIdent pyModName)  
                                    (Just (mkIdent pyalias))
                                    noSpan
                      ] noSpan)
        noSpan -}

combineStatements ::
    [Py.Statement SrcSpan]
    -> FullyPyProgram
    -> Py.Statement SrcSpan
    -> Py.Module SrcSpan
combineStatements originalStmts (Program _channelInits _ nodeFuns) multiMain = Py.Module combinedStatements
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
buildMain taskNames chnlsPerTask (Program chnls resExpr _tasks) params =  Py.Fun (mkIdent "main") params' resAnno mainBlock noSpan
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
paramToArg (Py.Param pId _mTyp _mDef _anno) = Py.ArgExpr (Py.Var pId noSpan) noSpan
paramToArg _ = error "unsupported parameter type. this should have been caught in the frontend"

replaceId:: (Py.ParameterSpan, Py.IdentSpan) -> Py.ParameterSpan
replaceId (Py.Param _id mTyp mDef anno, newId) = Py.Param newId mTyp mDef anno


modName :: Py.Parameter annot -> [Char] -> Py.Ident SrcSpan
modName (Py.Param (Py.Ident iName _) _mTyp _mDef _anno) modV = Py.Ident (iName ++ modV) noSpan


subToPython :: Program Stmt Stmt (Py.Statement SrcSpan) (Py.Expr SrcSpan) PythonVarType -> FullyPyProgram
subToPython (Program c r t ) =  Program (map subToStmt c) (subToExpr . unwrapSubStmt $ r) t

enumeratedTasks :: [FullTask (Py.Expr SrcSpan) PythonVarType (Py.Statement SrcSpan)] -> [String]
enumeratedTasks  tasks =  zipWith (\ _task i -> "task_" ++ show i) tasks [1..]

