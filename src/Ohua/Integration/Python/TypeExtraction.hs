{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeExtraction where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation


import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty


data PythonArgType = PythonObject
type PythonTypeAnno  = PythonArgType  
type FunTypes = HM.HashMap QualifiedBinding (FunType PythonTypeAnno)


-- We currently do none of this scope type extracction but 
-- determine the type of functions only at call side 
{--
extractFromFile :: CompM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)


extract :: forall m a. (CompM m, Show a) => FilePath -> Module a -> m (HM.HashMap QualifiedBinding (FunType PythonArgType))
extract srcFile (Module statements) = HM.fromList <$> extractTypes statements
    where
        extractTypes:: (CompM m, Show a) => [Statement a] -> m [(QualifiedBinding, FunType PythonArgType)]
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
            (Parameter a -> [ArgType PythonArgType] -> m (FunType PythonArgType)) -> 
            [Parameter a] -> 
            m (FunType PythonArgType)
        extractFunType convert params =  case params of
                [] -> return $ FunType $ Left Unit
                (x:xs) -> convert x  =<< mapM convertArg xs


        convertArg :: (CompM m, Show a) => Parameter a -> m (ArgType PythonArgType)
        convertArg param@Param{} = return $ Type $ annotation_or_error param
        convertArg argsP@VarArgsPos{}= throwError "Currently we can't type varargs"
        convertArg kargsP@VarArgsKeyword{}= throwError "Currently we can't type kwargs" 
        -- TODO: Can we somehow silently ignore EndPositional ? 
        convertArg EndPositional{} = throwError "Currently we do not support keyword-only argument"
        convertArg UnPackTuple{} = throwError "Found an 'UnPackTuple' token. This is a python 2 feature and not supposed to pass the python 3 parser used. Please contanct the author of language-python."
        

        annotation_or_error:: Parameter a -> PythonArgType
        -- Note: Let's pretend for a while there's only annotaded parameters in the world
        -- Note: Expr's could be anything but Expr's resulting from param_py_annotation can only be Var
        -- TODO: this should extract the string from var_ident, not the Span -> refactor that Span-type problem
        annotation_or_error param = case param_py_annotation param of
            Just Var{var_ident=typestring, expr_annot = a} -> PythonObject
            Nothing -> error $ "Some argment wasn't typed: " <> show (param_name param)
-}
