module Integrations.TestSetup where

import Ohua.Commons.Prelude 
import Ohua.Core.Types.Environment (Options)
import TestOptions (withRec, DebugOptions (..), CompilationType(..))

import Data.Text as T (Text, concat)
-- TO be testable, we need for all integrations 
-- a) a function to render/show produced source code
-- b) a function to parse source code from QQ or file
-- d) a 'core' compileCode'function
-- Specific for all integrations are 


class Testable codeFormat where
    -- Todo: Replace direct type parameter use by CodeFormat constr,
    type CodeFormat codeFormat :: Type

    compileFormat:: codeFormat -> Options -> CompilationType -> ReaderT DebugOptions IO codeFormat
    renderFormat:: codeFormat -> T.Text


    -- I don't think we want to add a CompileType to every single test 
    -- I suggest wrapping that into a method and adding it to Optipns instead
    -- and in case we do for any reason, we can still directly use compileFormat

    -- TODO: Using def twice seems weard ... Can we clean that up?
    compileCode :: codeFormat -> IO codeFormat
    compileCode inCode = runReaderT (compileFormat inCode def OhuaOnly) def 

    compileCodeWithRec :: codeFormat -> IO codeFormat
    compileCodeWithRec inCode = runReaderT (compileFormat inCode (withRec def) OhuaOnly) def

    compileCodeWithRecWithDebug :: codeFormat -> IO codeFormat
    compileCodeWithRecWithDebug inCode = runReaderT (compileFormat inCode (withRec def) OhuaOnly) $ DebugOptions True False

    compileCodeWithDebug :: codeFormat -> IO  codeFormat
    compileCodeWithDebug inCode = runReaderT (compileFormat inCode def OhuaOnly) $ DebugOptions True False

    compileAndRun :: codeFormat -> IO codeFormat
    compileAndRun inCode = runReaderT (compileFormat inCode def RunTarget) $ DebugOptions False False
    
    showCode :: T.Text -> codeFormat -> IO T.Text
    showCode msg code = runReaderT (showFormat msg code) def

    showCodeWithDiff :: T.Text -> codeFormat -> IO T.Text
    showCodeWithDiff msg code = runReaderT (showFormat msg code) $ DebugOptions False True

    showFormat:: T.Text -> codeFormat -> ReaderT DebugOptions IO T.Text
    showFormat msg ast =
        let
            c = renderFormat ast
        in do
            showDiff <- asks showCodeDiff
            lift $ when showDiff $ printCode c
            return c
        where
            printCode c = putStr $ boundary <> header <> c <> boundary
            boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
            header = msg <> "\n\n"
