{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.Core.ALang.PPrint
    ( Pretty(pretty)
    , prettyExpr
    ) where

import Ohua.Core.Prelude

import Control.Comonad (extract)
import Control.Comonad.Trans.Cofree (headF, tailF)
import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc

import Ohua.Core.ALang.Lang

afterLetIndent :: Int
afterLetIndent = 0

argumentIndent :: Int
argumentIndent = 2

data Prescedence
    = VarPrec
    | AppPrec
    | BindPrec
    | LamPrec
    | LetPrec
    deriving (Eq, Ord)

type WPrec a = (Doc a, Prescedence)

parenthesize :: Prescedence -> WPrec a -> Doc a
parenthesize prec1 (e, prec0)
    | prec0 > prec1 = parens e
    | otherwise = e
noParens :: Doc a -> WPrec a
noParens = (, VarPrec)
needParens :: Prescedence -> Doc a -> WPrec a
needParens prec = (, prec)
discardParens :: WPrec a -> Doc a
discardParens = fst

prettyExpr :: Expr embExpr ty -> Doc a
prettyExpr = fst . histo worker
  where
    worker =
        \case
            VarF tbnd -> noParens $ pretty tbnd
            LitF l -> noParens $ pretty l
            LetF assign expr (extract -> cont) ->
                let (assigns, e) = collectLambdas expr
                 in needParens LetPrec $
                    sep
                        [ "let" <+>
                          align
                              (hang afterLetIndent $
                               sep
                                   [ hsep (map pretty $ assign : assigns) <+>
                                     "="
                                   , discardParens e
                                   ]) <+>
                          "in"
                        , align (discardParens cont)
                        ]
            ApplyF (extract -> fun) (extract -> arg) ->
                needParens AppPrec $
                hang argumentIndent $
                sep [parenthesize AppPrec fun, parenthesize VarPrec arg]
            LambdaF assign body ->
                let (assigns, e) = collectLambdas body
                 in needParens LamPrec $
                    "λ" <+>
                    align
                        (sep [ sep (map pretty (assign : assigns) <> ["->"])
                             , discardParens e
                             ])
            BindStateF (extract -> s) (extract -> fun) ->
                needParens BindPrec $
                hsep [discardParens fun, "with", discardParens s]
    collectLambdas =
        para $ \case
            (tailF -> LambdaF assign (_, (assigns, e))) -> (assign : assigns, e)
            (headF -> other) -> ([], other)

instance Pretty (Expr embExpr ty) where
    pretty = prettyExpr

instance Pretty SomeBinding where
    pretty (Qual q) = pretty q
    pretty (Unqual b) = pretty b

instance Pretty (TypedBinding ty) where
    pretty (TBind bnd ty) = pretty bnd <> "::" <> show ty
