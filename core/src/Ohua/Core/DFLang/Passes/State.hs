module Ohua.Core.DFLang.Passes.State where

import Ohua.Core.Prelude
import Ohua.Core.DFLang.Lang
import Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.Compile.Configuration

-- | This optimization is optional. It needs to be enable by integrations that do __not__
--   have immutable data and as such state can not "live" on separate nodes.
--   The transformation evicts the size input and thereby enforces that the backend
--   needs to fuse this node with the stateful function. The size is then taken from the
--   state which is also contexted and has the size argument already.
--
--   Note: The backend can simply drop this input because then the channel is not being
--   created while there are still the senders which now use a channel that is not being
--   created anymore. Doing it here at DFLang level will allow the dead code elimination to
--   take care of the senders.
intoFusable :: Monad m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
intoFusable = mapFunsM (pure . f)
  where
    f (PureDFFun out r@(FunRef fun _ _) (_:| [sIn]))
      | fun == Refs.runSTCLangSMap = PureDFFun out r (sIn :| [])
    f e = e

load :: CustomPasses ty -> CustomPasses ty
load (CustomPasses alang alangNorm dfLang) =
  CustomPasses alang alangNorm (dfLang >=> intoFusable)
