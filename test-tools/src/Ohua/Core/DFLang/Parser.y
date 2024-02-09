{
-- |
-- Module      : $Header$
-- Description : Parser for DFLang
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections, TypeFamilies, FlexibleContexts, DataKinds #-}
module Ohua.Core.DFLang.Parser
    ( parseExp
    ) where

import Ohua.Prelude

import qualified Data.Sequence as Seq

import Ohua.Core.DFLang.Lexer
import Ohua.Core.DFLang.Lang

import Prelude ((!!))

import Language.Haskell.TH.Syntax (Lift)
import Data.Data

}

-- ToDo: This won't work any more since we eliminated 'Untyped' functions and 'TypeVar' aspossible varaible type

%name parseExpRaw Exp
%tokentype { Lexeme }
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    id              { UnqualId $$ }
    qualid          { QualId $$ }
    nsid            { ModuleId $$ }
    int             { Int_ $$ }
    env_ref         { EnvRef $$ }

    let             { KWLet }
    in              { KWIn }
    '('             { LParen }
    ')'             { RParen }
    '['             { LBracket }
    ']'             { RBracket }
    '<'             { LAngle }
    '>'             { RAngle }
    '='             { OPEq }
    ','             { OPComma }

%%
unit : '('')' {}

many1 (p)
    : p many1(p) { $1 : $2 }
    | p          { [$1] }

many (p)
    : many1(p)  { $1 }
    |           { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { $1 : $3 }
    | p                       { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

tuple(p)
    : '(' many_sep(p, ',') ')' { $2 }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

ModId :: { NSRef }
ModId
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Exp :: { NormalizedExpr NoType }
Exp : LetExpr Exp   { $1 $2 }
    | id            { Var $1 }

LetExpr :: { NormalizedExpr NoType -> NormalizedExpr NoType }
LetExpr : let Pat '=' FnRef opt(StateArg) tuple(DFVar) in 
    { 
        let outs = $2 in
        let fun = $4 in
        let state = $5 in
        let inp = case $6 of
                    [] -> DFEnvVar TypeUnit UnitLit :| []
                    (a:r) -> a :| r
        in case state of
            Just s -> Let $ StateFun outs fun (DFVar TypeVar s) inp
            Nothing -> case outs of 
                        (Nothing, out) -> Let $ PureFun out fun inp
                        _ -> error "Wrong output format. Only single result allowed."
    }

DFVar :: { DFVar 'Data NoType }
DFVar
    : Lit { DFEnvVar TypeVar $1 }
    | id  { DFVar TypeVar $ DataBinding $1 }

Lit :: { Lit NoType }
    : int { NumericLit $1 }
    | env_ref { EnvRefLit $1 }
    | unit { UnitLit }
    | qualid { FunRefLit $ FunRef $1 Untyped }

FnRef :: { QualifiedBinding }
FnRef : qualid { $1 }

StateArg :: { ABinding 'State }
StateArg : '[' id ']' { StateBinding $2 }

Pat :: {( Maybe (ABinding 'State), ABinding 'Data) }
Pat : '(' id ',' id ')' { (Just $ StateBinding $2, DataBinding $4) }
    | id { (Nothing, DataBinding $1)}

{

data NoType

instance Data NoType
instance Data (NormalizedExpr NoType)
instance Lift NoType
instance Lift (NormalizedExpr NoType)

type Pat = [Binding]
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

parseExp :: Input -> NormalizedExpr NoType
parseExp = runPM parseExpRaw

}
