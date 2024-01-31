module Ohua.Core.ALang.Passes.Verify where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang


ensureNthStructure :: MonadOhua m => Expr embExpr ty -> m ()
ensureNthStructure e =
    mapM_
        (failWith .
         ("Missing total length argument to nth call bound to " <>) . show)
        [ x
        | Let x (Apply (Lit (FunRefLit (FunRef "ohua.lang/nth" _))) _e1) _ <-
              universe e
        ]

checkInvariants :: MonadOhua m => Expr embExpr ty -> m ()
checkInvariants = ensureNthStructure
