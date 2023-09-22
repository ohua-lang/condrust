module Integrations.Python.CodeSamples.SimpleQuoter where

import Ohua.Prelude

import qualified Language.Python.Version3 as V3
import qualified Language.Python.Version2 as V2

import Language.Python.Common.Token as Token
import Language.Python.Common.AST (ModuleSpan)
import Language.Python.Common.ParserMonad ( ParseError)

import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ)


type Parser  = String -> String -> Either ParseError (ModuleSpan , [Token])

quoter :: Parser  -> QuasiQuoter
quoter parser = QuasiQuoter
-- any of the quasiQuoters should be a function that takes a String and returns
-- a 'Q' (short for Quasi) of Exp, Pat, Dec or Type respectively
                { quoteExp = parseAndLift parser "embedded"
                , quotePat = error "this quasiquoter does not support patterns"
                , quoteDec = error "this quasiquoter does not support declarations"
                , quoteType = error "this quasiquoter does not support types"
                }


parseAndLift:: Parser -> String -> String -> Q Exp
parseAndLift parser pname content = do 
  parseResult <- apply parser pname content
  dataToExpQ (const Nothing) parseResult 

apply:: (Monad m, MonadFail m) => Parser -> String -> String -> m ModuleSpan
apply parser pname content  = 
  case parser content pname of
    Left parse_error -> fail $ show parse_error
    Right (ast, _comments) -> return ast


pythonModule :: QuasiQuoter
pythonModule = quoter V3.parseModule

python2Module :: QuasiQuoter
python2Module = quoter V2.parseModule