module Ohua.Core.ALang.Passes.Verify where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang


-- FIXME Why does this not work?
-- ensureNthStructure :: MonadOhua m => Expression -> m ()
-- ensureNthStructure =
--     para $ \case
--         Apply (Apply (Lit (FunRefLit (FunRef "Ohua.Core.lang/nth" Nothing))) (Lit (NumericLit _))) (Var _) ->
--             failWith $ "Missing total length argument to nth "
--         e -> return ()
ensureNthStructure :: MonadOhua m => Expression -> m ()
ensureNthStructure e =
    mapM_
        (failWith .
         ("Missing total length argument to nth call bound to " <>) . show)
        [ x
        | Let x (Apply (Lit (FunRefLit (FunRef "Ohua.Core.lang/nth" Nothing))) e1) _ <-
              universe e
        ]

checkInvariants :: MonadOhua m => Expression -> m ()
checkInvariants = do
    ensureNthStructure
