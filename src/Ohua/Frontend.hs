module Ohua.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Types
import Ohua.Frontend.Lang
import Ohua.Frontend.Transform.Load
import Ohua.Frontend.Transform.Calls
import Ohua.Frontend.Transform.Maps
import Ohua.Frontend.Transform.Resolve


frontend :: (CompM m, Integration lang) => lang -> CompilationScope -> FilePath -> m (lang, Namespace Expr)
frontend lang compScope inFile = do
    (lang', ns) <- load lang compScope inFile
    ns' <- resolveNS ns
    -- ns'' <- transform ns'
    return (lang', ns')
    -- TODO
    -- where
    --     transform = mapM 
    --         (\algo -> 
    --             ((algo,) . 
    --             convertMaps . -- TODO
    --             makeImplicitFunctionBindingsExplicit) (algo^.algoCode))
    --         . (^. algos)