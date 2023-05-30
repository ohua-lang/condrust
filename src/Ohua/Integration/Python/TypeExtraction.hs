{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Integration.Python.TypeExtraction where

import Ohua.Prelude
import Ohua.Integration.Python.Util

import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation


import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty


data PythonVarType = PythonObject deriving Show
type PythonTypeAnno  = PythonVarType  
type FunTypes = HM.HashMap QualifiedBinding (FunType PythonTypeAnno)


-- We currently do none of this scope type extracction but 
-- determine the type of functions only at call side 
{--
extractFromFile :: ErrAndLogM m => FilePath -> m FunTypes
extractFromFile srcFile = extract srcFile =<< liftIO (load srcFile)


extract :: forall m a. (ErrAndLogM m, Show a) => FilePath -> Module a -> m (HM.HashMap QualifiedBinding (FunType PythonVarType))
extract srcFile (Module statements) = HM.fromList <$> extractTypes statements
    where
        extractTypes:: (ErrAndLogM m, Show a) => [Statement a] -> m [(QualifiedBinding, FunType PythonVarType)]
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

        extractFunType :: (ErrAndLogM m, Show a) => 
            (Parameter a -> [VarType PythonVarType] -> m (FunType PythonVarType)) -> 
            [Parameter a] -> 
            m (FunType PythonVarType)
        extractFunType convert params =  case params of
                [] -> return $ FunType $ Left Unit
                (x:xs) -> convert x  =<< mapM convertArg xs


        convertArg :: (ErrAndLogM m, Show a) => Parameter a -> m (VarType PythonVarType)
        convertArg param@Param{} = return $ Type $ annotation_or_error param
        convertArg argsP@VarArgsPos{}= throwError "Currently we can't type varargs"
        convertArg kargsP@VarArgsKeyword{}= throwError "Currently we can't type kwargs" 
        -- TODO: Can we somehow silently ignore EndPositional ? 
        convertArg EndPositional{} = throwError "Currently we do not support keyword-only argument"
        convertArg UnPackTuple{} = throwError "Found an 'UnPackTuple' token. This is a python 2 feature and not supposed to pass the python 3 parser used. Please contanct the author of language-python."
        

        annotation_or_error:: Parameter a -> PythonVarType
        -- Note: Let's pretend for a while there's only annotaded parameters in the world
        -- Note: Expr's could be anything but Expr's resulting from param_py_annotation can only be Var
        -- TODO: this should extract the string from var_ident, not the Span -> refactor that Span-type problem
        annotation_or_error param = case param_py_annotation param of
            Just Var{var_ident=typestring, expr_annot = a} -> PythonObject
            Nothing -> error $ "Some argment wasn't typed: " <> show (param_name param)
-}
