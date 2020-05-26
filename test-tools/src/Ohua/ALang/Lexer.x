{
-- |
-- Module      : $Header$
-- Description : Lexer for ALang
-- Copyright   : (c) Justus Adam 2018. Sebastian Ertel 2020. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-unused-imports #-}
module Ohua.ALang.Lexer (tokenize, Lexeme(..), Alex, Input, getLexerPos, alexMonadScan, runAlex, alexError) where

import Ohua.Prelude hiding (undefined)

import qualified Data.ByteString.Lazy.Char8 as BS

import Prelude (undefined, read)
}

%wrapper "monad-bytestring"

$char = [a-zA-Z]
$sym  = [\-\>\<\$\*\+\?\~\^\=_]
$num_not_zero = [1-9]
$numerical = [$num_not_zero 0]
$reserved = [@\#\{\}\[\]]
$idstartchar = [$char _]
$idchar = [$numerical $idstartchar]
$sep = [$white]

@id = $idstartchar $idchar*
@ns = @id (\. @id)*

@number = $num_not_zero $numerical* | 0


:-
<0> {
    "(*"            { begin comment }
    "("             { direct LParen }
    ")"             { direct RParen }
    "{"             { direct LBrace }
    "}"             { direct RBrace }
    "let"           { direct KWLet }
    "in"            { direct KWIn }
    "if"            { direct KWIf }
    "then"          { direct KWThen }
    "else"          { direct KWElse }
    "algo"          { direct KWAlgo }
    "sf"            { direct KWSf }
    "with"          { direct KWWith }
    "="             { direct OPEq }
    ":"             { direct OPColon }
    ";"             { direct OPSemicolon }
    ";;"            { direct OPDoubleSemicolon }
    ","             { direct OPComma }
    "->"            { direct OPArrow }
    "=>"            { direct OPDoubleArrow }
    "\" | "λ"       { direct OPLambda } -- "
    "-"             { direct OPMinus }
    @number         { tokenOverInputStr $ Number . read . BS.unpack }
    "$" @number     { tokenOverInputStr $ EnvRef . makeThrow . read . BS.unpack . BS.tail }
    @id             { tokenOverInputStr $ UnqualId . convertId }
    @ns\/@id        { tokenOverInputStr $ QualId . mkQualId }
    $sep            ;

    $reserved       { withMatchedInput $ \s -> alexError $ "Reserved symbol: " <> decodeUtf8 s }
}

<comment> {
    "*)"      { begin 0 }
    . ;
    \n ;
}

{
type Input = BS.ByteString

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | LBrace -- ^ @{@
    | RBrace -- ^ @}@
    | KWLet -- ^ keyword @let@
    | KWIn -- ^ keyword @in@
    | KWIf -- ^ keyword @if@
    | KWThen -- ^ keyword @then@
    | KWElse -- ^ keyword @else@
    | KWAlgo -- ^ keyword @algo@
    | KWSf -- ^ keyword @sf@
    | KWWith
    | OPEq -- ^ operator @=@
    | OPColon -- ^ operator @:@
    | OPSemicolon -- ^ operator @;@
    | OPDoubleSemicolon -- ^ operator @;;@
    | OPComma -- ^ operator @,@
    | OPArrow -- ^ operator @->@
    | OPDoubleArrow -- ^ operator @=>@
    | OPLambda -- ^ operator @\\@ or @λ@
    | OPMinus
    | Number Integer
    | EnvRef HostExpr -- ^ a reference to an env expression
    | UnqualId Binding -- ^ an identifier
    | QualId QualifiedBinding -- ^ a qualified binding
    | EOF
    deriving Show

withMatchedInput f (_, _, input, _) len = f (BS.take len input)
tokenOverInputStr f = withMatchedInput (pure . f)

direct tok _ _ = pure tok
alexEOF = pure EOF

convertId :: ByteString.ByteString -> Binding
convertId = makeThrow . decodeUtf8

tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either (error . toText) id $ runAlex bs $
  let go = alexMonadScan >>= \case EOF -> pure []; tok -> (tok:) <$> go
  in go

getLexerPos :: Alex (Int, Int)
getLexerPos = Alex $ \s@AlexState{ alex_pos=AlexPn _ line col} -> pure (s, (line, col))

mkQualId :: BS.ByteString -> QualifiedBinding
mkQualId str = QualifiedBinding (mkNSRef nsstr) (convertId name0)
  where
    (nsstr, name') = BS.break (== '/') str
    name0 = BS.tail name'


mkNSRef :: BS.ByteString -> NSRef
mkNSRef = makeThrow . map convertId . BS.split '.'


}
