module Ohua.Backend.Fusion.Util where


import Ohua.Commons.Prelude

import Ohua.Backend.Lang
import qualified Ohua.Backend.Operators.Function as F
import Ohua.Backend.Operators.Common

import qualified Data.HashSet as HS
import qualified Data.Foldable as DF
import qualified Data.List.NonEmpty as NE


-- | This function filters the inputs by checking whether the outputs
--   have the same channel reference. If they do then clearly we can drop it from the list of
--   inputs such that no receive code is being generated.
filterData :: [Com 'Channel embExpr ty] -> [VarReceive embExpr ty] -> [VarReceive embExpr ty]
filterData [] ins0 = ins0
filterData outs ins0 =
  filter
  ((\(SRecv _ inp) -> not $ HS.member inp $ HS.fromList outs) .
   snd .
   fromVarReceive)
  ins0

forPure :: (DF.Foldable t, Functor t)
        => t (F.Result embExpr ty)
        -> Maybe (Com 'Channel embExpr ty)
        -> [VarReceive embExpr ty]
        -> ([VarReceive embExpr ty], (t (F.Result embExpr ty), Maybe (Com 'Channel embExpr ty)))
forPure out upstreamStateOut downstreamIns =
  let
    outAsList = DF.toList out
    downstreamInsNoState = filterData (maybeToList upstreamStateOut) downstreamIns
    sOut' = if length downstreamInsNoState < length downstreamIns then Nothing else upstreamStateOut
    downstreamIns'' = filterData (concatMap F.channels outAsList) downstreamInsNoState
    downstreamInChans = HS.fromList $ map ((\(SRecv _ inp) -> inp) . snd . fromVarReceive) downstreamIns

    -- | We drop all send operations for wich we can find inputs
    --   for the downstream operator that we are fusing.
    --   Because all we want to do is bind this result to a value but not send it.
    --   The above function `filterData` does the exact same thing for the inputs
    --   of the downstream operator.
    drop F.DropResult       = F.DropResult
    drop o@(F.SendResult c) = if HS.member c downstreamInChans
                              then F.DropResult
                              else o
    drop (F.DispatchResult cs) =
      let cs' = NE.filter (not . (`HS.member` downstreamInChans)) cs
       in case cs' of
            []        -> F.DropResult
            [c]       -> F.SendResult c
            (c : cs'') -> F.DispatchResult $ c :| cs''

    out' = map drop out
  in (downstreamIns'', (out', sOut'))
