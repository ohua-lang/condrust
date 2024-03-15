module Ohua.Frontend.Transform.UnitArgs where

import Ohua.Commons.Prelude

import Ohua.Frontend.Lang

noEmptyArgs :: ResolvedExpr embExpr annot ty -> FuncExpr embExpr annot ty
noEmptyArgs e =  case e of
            AppE fun annots args -> case args of
                []      -> AppE (noEmptyArgs fun) annots (LitE UnitLit :| [])
                (a:as)  -> AppE (noEmptyArgs fun) annots (map noEmptyArgs (a:| as))
            LamE pats fn -> case pats of 
                []      -> LamE (VarP "_" (IType TypeUnit) :| []) (noEmptyArgs fn)
                (p:ps)  -> LamE (p:|ps) (noEmptyArgs fn)
            StateFunE annots st meth args -> case args of 
                []      -> StateFunE annots (noEmptyArgs st) meth (LitE UnitLit :| []) 
                (a:as)  -> StateFunE annots (noEmptyArgs st) meth (map noEmptyArgs (a:| as))

            VarE b t -> VarE b t
            LitE l -> LitE l
            LetE p e1 e2 -> LetE p (noEmptyArgs e1) (noEmptyArgs e2)           
            IfE c te fe -> IfE (noEmptyArgs c) (noEmptyArgs te) (noEmptyArgs fe)
            WhileE c body -> WhileE (noEmptyArgs c) (noEmptyArgs body)
            MapE e1 e2 -> MapE (noEmptyArgs e1) (noEmptyArgs e2)
            StmtE e1 e2 -> StmtE (noEmptyArgs e1) (noEmptyArgs e2)
            TupE es -> TupE (map noEmptyArgs es)

