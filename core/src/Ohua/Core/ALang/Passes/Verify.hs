module Ohua.Core.ALang.Passes.Verify where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang


ensureNthStructure :: MonadOhua m => Expr ty -> m ()
ensureNthStructure e =
    mapM_
        (failWith .
         ("Missing total length argument to nth call bound to " <>) . show)
        [ x
        | Let x (Apply (Lit (FunRefLit (FunRef "ohua.lang/nth" Nothing _))) _e1) _ <-
              universe e
        ]

checkInvariants :: MonadOhua m => Expr ty -> m ()
checkInvariants = ensureNthStructure
