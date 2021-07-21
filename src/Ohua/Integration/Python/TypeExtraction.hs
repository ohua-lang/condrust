{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeExtraction where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation


import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty

--TODO: Provide definitions for function types and argument types
--TODO: Implement extracion of function types and argument types in a given NS (py-module)
--Note: OHUA functions types may be Untyped, FunType(d) or STFunType(d), last one being functions with self arg. 
--      in Rust
-- TODO: Fun.Parameters in AST are just Expr's. Define of Expr's eg. Immutables | Structures | Callables | Self??
-- Note: For whatever reason there is a separate Type Argument that is not used to define Fun :-/
data PythonArgType = PythonObject
type PythonTypeAnno = PythonArgType
type FunTypes = HM.HashMap QualifiedBinding (FunType PythonTypeAnno)


extractFromFile :: CompM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)

-- TODO: Given a filepath and an AST, ceate a Hashmap of contextualized funtion names (qualified bindings) and function types
-- function types 
-- Note: in Rust 'self' funtions are separate items (Impl) -> in Python identify via context (in a Suite of Class items)
-- and expl. parse first arg as self is not a type in the parser 

extract :: forall m a. (CompM m, Show a) => FilePath -> Module a -> m (HM.HashMap QualifiedBinding (FunType (PythonArgType a)))
extract srcFile (Module statements) = HM.fromList <$> extractTypes statements
    where
        extractTypes:: (CompM m, Show a) => [Statement a] -> m [(QualifiedBinding, FunType (PythonArgType a))]
        extractTypes statements = 
            catMaybes . concat <$>
            mapM
                (\case
                    function@Fun{} -> 
                        (: []) . Just . (createFunRef (fun_name function), ) 
                        <$> extractFunType (\x xs -> FunType . Right . (:|xs) <$> convertArg x) (fun_args function)
                    _ -> return [])
                statements

        createFunRef :: Ident a-> QualifiedBinding
        createFunRef = 
            QualifiedBinding (filePathToNsRef srcFile) . toBinding

        extractFunType :: (CompM m, Show a) => 
            (Parameter a -> [ArgType (PythonArgType a)] -> m (FunType (PythonArgType a))) -> 
            [Parameter a] -> 
            m (FunType (PythonArgType a))
        extractFunType convert params =  case params of
                [] -> return $ FunType $ Left Unit
                (x:xs) -> convert x  =<< mapM convertArg xs

        -- TODO: Instead of just ignoring anything ... parse types of parameters 

        convertArg :: (CompM m, Show a) => Parameter a -> m (ArgType (PythonArgType a))
        convertArg param@Param{} = return $ Type $ annotation_or_error param
        convertArg argsP@VarArgsPos{}= throwError "Currently we can't type varargs"
        convertArg kargsP@VarArgsKeyword{}= throwError "Currently we can't type kwargs" 
        -- TODO: other patterns are EndPositional (not a parameter but the end of varargs) and
        -- UnPackTuple, the former will not produce anything so wrap output in Maybe? 

        annotation_or_error:: Parameter a -> PythonArgType a
        -- Note: Let's pretend for a while there's only annotaded parameters in the world
        annotation_or_error param = case param_py_annotation param of
            Just var_expression -> var_expression
            Nothing -> error $ "Some argment wasn't typed: " <> show (param_name param)
