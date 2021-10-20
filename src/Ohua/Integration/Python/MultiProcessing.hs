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

import System.FilePath (takeFileName)

type FullyPyProgram = Program (Py.Statement SrcSpan) (Py.Expr SrcSpan) (Py.Statement SrcSpan) PythonArgType

instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python
    type Chan (Architectures 'MultiProcessing) = Sub.Stmt
    type ATask (Architectures 'MultiProcessing) = Py.Statement SrcSpan

    -- | convert a backend channel i.e. an arc in the DFG to an expression of the target architecture
    --  instantiating the according process communication channel
    convertChannel SMultiProc (SRecv argTy( SChan bnd))=
        let expr = unwrapSubStmt $ convertExpr SMultiProc $ Apply $ Stateless (QualifiedBinding (makeThrow []) "mp.Pipe") []
            send = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_sender"
            recv = unwrapSubStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_receiver"
        in Sub.Assign [Sub.Tuple [send, recv]] expr

    -- | converts an 'incomming edge' of a backend channel into an expression of the target architecture
    --  to receive from a process communication channel 
    -- Todo: Rust wraps that in a 'try'. Receiving is blocking 
    -- and raises EOFError if there's nothing left to receive and the sender is allready closed
        -- > Do I need to wrap this also?
    convertRecv SMultiProc  (SRecv _type (SChan channel)) =
    -- currently this will yield $channel_reciever.recv()           
        convertExpr SMultiProc $
            Apply $ Stateful (TCLang.Var $ channel <> "_receiver") (mkFunRefUnqual "recv") []

    -- | Converts the 'outgoing edge' of a backend channel into an expression of the target architecture
    --  to send the result of the node computation to a process communication channel 
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

{-    serialize :: 
        ( CompM m
        , Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , expr ~ (Expr (Lang arch))
        )
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang)
        -> m (NonEmpty (FilePath, L.ByteString))
-}
    -- Generiert Python Code aus dem neuen AST 
    -- Alles was auf verteilten physischen Konten laufen soll, sollte auch in verschiedene Dateien.
    -- Für Multiprocessing reicht erstmal eine Datei, aber zB bei CloudMicroservices währen Tasks deutlich unabhängiger 
    -- (eigene Module, eigene Imports etc.)

    -- in Rust-Integration serialzation just takes over the original module and replaces
    -- the body of each function definition with the whole DFG code obviously
    -- In Python, it can't be done this way because of point 2 in the todos 
        --TODO:
        -- 1. import multiprocessing
        -- 2. tasks and channel inits NEED TO BE TL. Multiprocessing pickles, and 
        ---   pickling only works for TL definitions
        -- 3. build a new main function 
            -- has renamed parameters of original algorithm
            -- declares variables named according to the original parameters of the algorithm global
            -- assigns it' according parameters to those globals
            -- inits tasks list and assign processes to each task
            -- start processes and receive result
            -- terminate and join processes
            -- return result
        -- Critical: receiving the result must happen before termination, but is blocking 
    serialize  SMultiProc srcModule ns = outPut algoNames newModules
        where
            namesProgramsSources = map (\(Algo name expr src) -> (name, expr, src)) $ ns^.algos
            algoNames = map (^. algoName) $ ns^.algos
            originalFunctions = map  (^. algoAnno) $ ns^.algos
            original_params = map Py.fun_args originalFunctions
            programs = map  (^. algoCode) $ ns^.algos
            converted = map subToPython programs
            taskLists = map (\(Program _ _ tasks) -> enumeratedTasks tasks) programs
            newMains = map buildMain $ zip3 taskLists converted original_params
            newModules = zipWith (curry (makeModule srcModule)) converted newMains

subToPython :: Program Stmt Stmt (Py.Statement SrcSpan) PythonArgType
    -> FullyPyProgram
subToPython (Program c r t ) =  Program (map subToStmt c) (subToExpr . unwrapSubStmt $ r) t

enumeratedTasks :: [FullTask PythonArgType (Py.Statement SrcSpan)] -> [String]
enumeratedTasks  tasks =  zipWith (\ task i -> "task_" ++ show i) tasks [1..]


instance Transform (Architectures 'MultiProcessing)


makeModule ::
    Module
    -> (FullyPyProgram , Py.Statement SrcSpan)
    -> Module
makeModule srcModule (Program channelInits _ nodeFuns, multiMain) = Module path combinedModule
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
        combinedModule = Py.Module combinedStatements

buildMain:: ([String], FullyPyProgram, [Py.Parameter SrcSpan])-> Py.Statement SrcSpan
buildMain (taskNames, Program chnls resExpr tasks, params) =  Py.Fun (mkIdent "main") params' resAnno mainBlock noSpan
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


outPut ::CompM m => [Binding]-> [Module] -> m (NonEmpty (FilePath, L.ByteString))
outPut originalFunNames ((Module path pyModule): modules) =
    let fileName = takeFileName path
        pyCode = encodeUtf8 $ prettyText pyModule
    -- Question: Why do we both do this NonEmpty fuzz here ?
    in return $ (fileName, pyCode) :| []

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


modName (Py.Param (Py.Ident name _) mTyp mDef anno) modV = Py.Ident (name++modV) noSpan

replaceId:: (Py.ParameterSpan, Py.IdentSpan) -> Py.ParameterSpan
replaceId (Py.Param id mTyp mDef anno, newId) = Py.Param newId mTyp mDef anno