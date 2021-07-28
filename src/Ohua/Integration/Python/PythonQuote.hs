{-# LANGUAGE GADTs #-}
module PythonQuote where

import Language.Python.Version3 as V3

import Language.Python.Common.Token as Token
import Language.Python.Common.AST (ModuleSpan, Expr, ExprSpan, StatementSpan)
import Language.Python.Common.ParserMonad ( ParseError)
import Language.Python.Common.Pretty (prettyText, prettyPrefix, Pretty)

import Data.Data ( Data )
import Language.Haskell.TH 
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )
import Language.Haskell.TH.Syntax (liftData)


type Parser  = String -> String -> Either ParseError (ModuleSpan, [Token])

quoter :: (Data a) => (String -> String -> Either ParseError (a, [Token]))-> QuasiQuoter
quoter parser = QuasiQuoter
-- any of the quasiQuoters should be a function that takes a String and returns
-- a 'Q' (short for Quasi) of Exp, Pat, Dec or Type respectively
                { quoteExp = parseAndLift parser "testname"
                , quotePat = error "this quasiquoter does not support patterns"
                , quoteDec = error "this quasiquoter does not support declarations"
                , quoteType = error "this quasiquoter does not support types"
                }


parseAndLift:: (Data a) => (String -> String -> Either ParseError (a, [Token])) -> String -> String -> Q Exp
parseAndLift parser name content = do 
  parseResult <- apply parser name content
  dataToExpQ (const Nothing) parseResult 

apply:: (Monad m, MonadFail m) => 
        (String -> String -> Either ParseError (a, [Token])) 
         -> String -> String -> m a
apply parser name content  = 
  case parser content name of
    Left parse_error -> fail $ show parse_error
    Right (ast, comments) -> return ast


pythonModule :: QuasiQuoter
pythonModule = quoter V3.parseModule


pythonStmt :: QuasiQuoter
pythonStmt = quoter V3.parseStmt


pythonExpr:: QuasiQuoter
pythonExpr = quoter V3.parseExpr
