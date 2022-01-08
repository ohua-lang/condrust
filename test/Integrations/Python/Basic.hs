module Integrations.Python.Basic where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Basics" $ do
        
        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        
        it "Assignment binary Operation of boolean literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBools) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBools
                compiled `shouldBe` expected) 
        
        it "Assignment binary Operation of vars" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOpChained) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOpChained
                compiled `shouldBe` expected)

        it "Assignment and return a variable" $
            (showCode "Compiled: " =<< compileCode Input.varReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.varReturn
                compiled `shouldBe` expected)     

        it "return function call" $
            (showCode "Compiled: " =<< compileCode Input.onlyReturnFunCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.onlyReturnFunCall
                compiled `shouldBe` expected) 

        it "Chained Assignment" $
            (showCode "Compiled: " =<< compileCode Input.chainedAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.chainedAssignment
                compiled `shouldBe` expected)
        
        it "Nested function calls" $
            (showCode "Compiled: " =<< compileCode Input.nestedCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.nestedCompose
                compiled `shouldBe` expected) 

        it "Assignments, Call function, Assign,  Return Var" $
            (showCode "Compiled: " =<< compileCode Input.assignCallAssignReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignCallAssignReturn
                compiled `shouldBe` expected)

        it "Algo with params" $
            (showCode "Compiled: " =<< compileCode Input.algoWithParams) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algoWithParams
                compiled `shouldBe` expected) 

        it "Apply ĺambda expr" $
            (showCode "Compiled: " =<< compileCode Input.applyLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.applyLambdaExpr
                compiled `shouldBe` expected) 
        
        it "Assign ĺambda expr" $
            (showCode "Compiled: " =<< compileCode Input.assignLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignLambdaExpr
                compiled `shouldBe` expected) 

        it "Assign empty list" $
            (showCode "Compiled: " =<< compileCode Input.assignEmptyList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyList
                compiled `shouldBe` expected) 
        
        it "Assign list with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignList
                compiled `shouldBe` expected) 

        it "Assign empty dict" $
            (showCode "Compiled: " =<< compileCode Input.assignEmptyDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyDict
                compiled `shouldBe` expected) 


        it "Assign dict with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignDict
                compiled `shouldBe` expected) 

        it "Assign set with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignSet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignSet
                compiled `shouldBe` expected) 
        
        it "Tuple as argument" $
            (showCode "Compiled: " =<< compileCode Input.tupleArgumentCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleArgumentCall
                compiled `shouldBe` expected)

        
        

        
{-  TOFIX 
        {-
        uncaught exception: ErrorCall
       Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
        -}
        it "Simple function call" $
            (showCode "Compiled: " =<< compileCode Input.callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)


         {-
        uncaught exception: ErrorCall
        Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
        CallStack (from HasCallStack):
             error, called at src/Ohua/Backend/Normalize.hs:83:12 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        -}
        it "Assignment no Return" $
            (showCode "Compiled: " =<< compileCode Input.noReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noReturn
                compiled `shouldBe` expected)
        
        {-
        uncaught exception: ErrorCall
        Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
        CallStack (from HasCallStack):
             error, called at src/Ohua/Backend/Normalize.hs:83:12 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        -}  
        it "Assignment and return" $
            (showCode "Compiled: " =<< compileCode Input.emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.emptyReturn
                compiled `shouldBe` expected)

        {-
        uncaught exception: ErrorCall
        Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
        CallStack (from HasCallStack):
             error, called at src/Ohua/Backend/Normalize.hs:83:12 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        -}
        it "Assignment and return None" $
            (showCode "Compiled: " =<< compileCode Input.noneReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noneReturn
                compiled `shouldBe` expected)
        
        {-
        uncaught exception: ErrorCall
        Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
        CallStack (from HasCallStack):
             error, called at src/Ohua/Backend/Normalize.hs:83:12 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        -}
        it "Expression no Return" $
            (showCode "Compiled: " =<< compileCode Input.exprNoReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.exprNoReturn
                compiled `shouldBe` expected)

        -- [Error] Compiler invariant broken! Seq must have two inputs where the second is a literal:
        -- PureDFFun (Direct (DataBinding (Binding "a_0"))) (QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "seq")) (DFEnvVar TypeVar (NumericLit 5) :| [DFEnvVar TypeVar UnitLit])
        it "Assignment numeric Literal" $
            (showCode "Compiled: " =<< compileCode Input.assignNumLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignNumLit
                compiled `shouldBe` expected)      
        -- [Error] Expected var or apply expr, got Lit (NumericLit 5)
        -- CallStack (from HasCallStack):
        --throwErrorS, called at src/Ohua/Core/Util.hs:228:20 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Util
        --throwErrorDebugS, called at src/Ohua/Core/Monad.hs:39:12 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Monad
        -- failWith, called at src/Ohua/Core/Compile.hs:99:25 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Compile
        it "Assignment numeric Literal and return var" $
            (showCode "Compiled: " =<< compileCode Input.assignNumLitReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignNumLitReturn 
                compiled `shouldBe` expected)     
        
        
        -- Same Error as with all 'None Return tests'
        it "Assignments, Call function, Return Var" $
            (showCode "Compiled: " =<< compileCode Input.assignmentCallReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignmentCallReturn
                compiled `shouldBe` expected)

        
        {-
        -- Fails until I implement augmented assignments
        it "Assignment augmented" $
            (showCode "Compiled: " =<< compileCode Input.assignAugmented) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignAugmented
                compiled `shouldBe` expected)
        -}
-}