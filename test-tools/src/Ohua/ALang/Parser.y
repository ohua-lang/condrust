{
-- |
-- Module      : $Header$
-- Description : Parser for ALang ML-Expressions
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections, TypeFamilies, FlexibleContexts #-}
module Ohua.ALang.Parser
    ( parseExp
    ) where

import Ohua.Prelude

import Control.Lens (view)
import Ohua.ALang.Lexer
import Ohua.ALang.Lang
import Ohua.ParseTools.Refs (ifBuiltin, mkTuple, smapBuiltin, seqBuiltin)
import qualified Data.HashMap.Strict as HM
import qualified Ohua.ParseTools.Refs as Refs
import qualified Ohua.ALang.Refs as Refs
import Ohua.Unit (someUnitExpr)
import Data.List.NonEmpty (NonEmpty((:|)))

import Prelude ((!!))

}


%name parseExpRaw Exp
%tokentype { Lexeme }
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    id              { UnqualId $$ }
    qualid          { QualId $$ }
    envRef          { EnvRef $$ }
    number          { Number $$ }

    let             { KWLet }
    in              { KWIn }
    algo            { KWAlgo }
    sf              { KWSf }
    if              { KWIf }
    then            { KWThen }
    else            { KWElse }
    with            { KWWith }
    '('             { LParen }
    ')'             { RParen }
    '{'             { LBrace }
    '}'             { RBrace }
    '='             { OPEq }
    ':'             { OPColon }
    ';'             { OPSemicolon }
    ';;'            { OPDoubleSemicolon }
    ','             { OPComma }
    '->'            { OPArrow }
    'λ'             { OPLambda }
    '-'             { OPMinus }
    '_'             { UnqualId "_" }

%%

many1 (p)
    : p many(p) { $1 :| $2 }

many (p)
    : p many(p)  { $1 : $2 }
    |            { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { let x :| xs = $3 in $1 :| x:xs }
    | p                       { $1 :| [] }

many_sep(p, sep)
    : many_sep1(p, sep) { toList $1 }
    |                   { [] }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

-- Decl
--     :: { Decl }
--     : ValDecl { $1 }

-- ValDecl
--     :: { ValDecl }
--     : let LetRhs '=' Exp ';;' { let (pat1, f) = $2 
--                                 in case pat1 of
--                                     [bnd] -> (bnd, f $4)
--                                     xs -> error $ "Non-var patterns not allowed for top level bindings: " <> show xs }

SimpleExp
    :: { Exp }
    : '(' many_sep(Exp, ',') ')' { case $2 of
                                       [] -> Lit UnitLit
                                       [x] -> x
                                       xs -> foldl (\e arg -> e `Apply` arg) (Lit $ FunRefLit $ FunRef mkTuple Nothing) xs }
    | opt('-') number            { Lit $ NumericLit $ maybe id (const negate) $1 $2 }
    | envRef                     { Lit $ EnvRefLit $1 }
    | qualid                     { Lit $ FunRefLit $ FunRef $1 Nothing }
    | id                         { Var $1 }

Exp :: { Exp }
    : Exp SimpleExp             { Apply $1 $2 }
    | 'λ' many1(Pat) '->' Exp   { foldr (\e cont -> Lambda e cont) $4 $2 }
    | let Let in Exp            { $2 $4 }
    | if Exp then Exp else Exp  { ifBuiltin `Apply` $2 `Apply` $4 `Apply` $6 }
    | Exp ';' Exp               { Let "_" $1 $3 }
    | Exp with Exp              { BindState $1 $3 }
    | SimpleExp                 { $1 }

Let :: { Exp -> Exp }
    : LetRhs '=' Exp            { let (x, f) = $1 in Let x $ f $3 }

LetRhs
    :: { (Binding, Exp -> Exp) }
    : many1(Pat)                { let x :| xs = $1 
                                    in (x, 
                                        \a -> if null xs 
                                            then a 
                                            else foldr (\x' cont -> Lambda x' cont) a xs) }

Pat :: { Binding }
    : id                        { $1 }

{

-- type Decl = ValDecl
-- type ValDecl = (Binding, Exp)
type Exp = Expr
type PM = Alex

nextToken :: PM Lexeme
nextToken = alexMonadScan

lexer :: (Lexeme -> PM a) -> PM a
lexer cont = nextToken >>= cont

runPM :: PM a -> Input -> a
runPM ac bs = either (error . toText) id $ runAlex bs ac

parseError :: Lexeme -> PM a
parseError token = do
  (line, col) <- getLexerPos
  alexError $ ("Parse error at line " <> show line <> ", column " <> show col <> ", on token " <> show token :: String)

parseExp :: Input -> Expr
parseExp = runPM parseExpRaw

}