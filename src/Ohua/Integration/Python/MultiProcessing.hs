{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Python.MultiProcessing where

import Ohua.Prelude

import Ohua.Backend.Types
import Ohua.Backend.Lang as TCLang
import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Architecture
import Ohua.Integration.Python.Backend
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.Types


import qualified Language.Python.Common.AST as Py
import Language.Rust.Data.Ident (mkIdent)


instance Architecture (Architectures 'MultiProcessing) where
    type Lang (Architectures 'MultiProcessing) = Language 'Python
    type Chan (Architectures 'MultiProcessing) = Py.Statement ()
    type ATask (Architectures 'MultiProcessing) = Py.Statement ()


-- Question: In the backend definition a task is a block/suite, while in the architecture it's just an Expression. 
-- I assume this is because Backend constructs a BlockExpr to wrap a task

{-- Note: How to produce a 'task' in Python
        Observations: 1. a task will contain Statements, no exception
                      2. I can not wrap Statements in Expressions, contrary to Rust where there is a BlockExpr to wrap 
                         lists of Statements
                      3. Lambdas in Python realy only take one Expression, some nifty thing are possible though:
                         >>> stack = []
                         >>> g = lambda *_: (stack.append(y_0_out.recv()),
                                             stack.append(x_0_out.recv()),
                                             stack.append(fun3(stack.pop(), stack.pop())),
                                             z_0_in.send(stack.pop()))[0]
                        Everything inside the tuple will be evaluated, return value will be None as this 
                        is what statements return 
                        Further Staments are possible as defaults for variables: lambda x= x_0_out.recv():...
                        However I think neither of those is an option.

        Conclusion: I need Suits() ie. [Staments] only possible
                    1.  in named functions, so I need names
                    - > I could have a State to enumerate fun0 - funN but using a hash of the 
                        task to be translated would probably be easier 
                    2. tbc

--}
--  convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch
-- convert a backend channel i.e. an arc in the DFG to an expression of the target architecture
-- instantiating an accoridng process communication channel
    convertChannel SMultiProc (SChan bnd)=
        let stmt = Apply $
                    Stateless
                        -- Todo: replace with real QB as soon as I got ns extraction from 
                        -- imports right
                        (QualifiedBinding (makeThrow []) "mp.Pipe")
                        []
            send = unwrapStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_sender"
            recv = unwrapStmt $ convertExpr SMultiProc $ TCLang.Var $ bnd <> "_receiver"
        in Py.Assign {
             assign_to = [Py.Tuple [send, recv] noSpan],
             assign_expr = unwrapStmt $ convertExpr SMultiProc stmt,
             stmt_annot = noSpan}

--  convertRecv :: arch -> Com 'Recv (Type (Lang arch)) -> Expr (Lang arch)
-- Convert an 'incomming edge' of a backend channel into an expression of the target architecture
--  to receive from a process communivcation channel 
    -- Todo: Rust wraps that in a 'try'. Receiving is blocking 
    -- and raises EOFError if there's nothing left to receive and the sender is allready closed
        -- > Do I need to wrap this also?
    convertRecv SMultiProc  (SRecv _type (SChan channel)) =
    -- currently this will yield $channel_reciever.recv()           
        convertExpr SMultiProc $ Apply $ Stateful (Var $ channel <> "_receiver") (mkFunRefUnqual "recv") []

--  convertSend:: arch -> Com 'Send (Type (Lang arch)) -> Expr (Lang arch)
 -- Todo: Sending is only valid for picklable objects, i.e. basic Types, things def'd at TL of a module
    -- and ADTs thereof. Restriction on the Frontend should actualy prohibit non-TL def's. Also objects 
    -- must not exceed ~ 32MiB. I might need to catch PickleError's or ValueError's here
    convertSend SMultiProc  (SSend (SChan chnlName) toSend)=
        convertExpr SMultiProc $ Apply $ Stateful (Var $ chnlName <> "_sender") (mkFunRefUnqual "send") [Var toSend]

{-  build :: 
        ( Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , expr ~ (Expr (Lang arch))
        , CompM m)
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) expr (Task lang) ty) (AlgoSrc lang)
        -> m (Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang))
-}
    -- Wie wird eine 'Task' (Py.Suite) ausgeführt..
    build SMultiProc (Module fPath (Py.Module stmts)) ns =
         return $ ns & algos %~ map (\algo -> algo & algoCode %~ createTasksAndChannels)
        where
            createTasksAndChannels (Program chans retChan tasks) =
                Program chans retChan (map (pure (createTask [1..]) <$>) tasks)

            createTask ::Int -> Py.Suite ()-> Py.Statement()
            createTask num code=
               Py.Fun
                (Py.Ident ("node_"++show num) noSpan)
                []
                Nothing
                code
                noSpan--}

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
    serialize = undefined


