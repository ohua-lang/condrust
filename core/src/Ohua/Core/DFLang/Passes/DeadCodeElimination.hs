module Ohua.Core.DFLang.Passes.DeadCodeElimination where

import Ohua.Core.Prelude
import Ohua.Core.DFLang.Lang as L

import Data.Text.Lazy.IO as T

eliminate :: (MonadOhua m) => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
eliminate expr = do
  expr' <- f expr
  case L.length expr == L.length expr' of
    True -> pure expr'
    False -> eliminate expr'
  where
    f :: (MonadOhua m) => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
    f (Let app@(PureDFFun out fun _) cont) =
      case out `isUsedIn` cont of
        True -> Let app <$> f cont
        False -> warn fun >> (f cont)
    f (Let app@(StateDFFun (stateOut,dataOut) fun sIn dIn) cont) =
      Let
      (StateDFFun
       (dropOut cont =<< stateOut, dropOut cont =<< dataOut)
       fun sIn dIn) <$>
      f cont
    f v = pure v

    dropOut cont out =
      case (out `isUsedIn` cont) of
        True -> Just out
        False -> Nothing

    out `isUsedIn` e = or $ map (`isBndUsed` e) $ outBnds out

    isBndUsed bnd (Let app cont) = bnd `elem` (insDFApp app) || bnd `isBndUsed` cont
    isBndUsed bnd (Var result) = bnd == result

    warn :: (MonadOhua m) => QualifiedBinding -> m ()
    warn fun@(QualifiedBinding (NSRef ns) _) =
      case ns == ["ohua","lang"] of
        True -> return ()
        False -> warning fun

    warning :: (MonadOhua m) => QualifiedBinding -> m ()
    warning fun = liftIO $ T.putStrLn $ "[WARNING] The output of pure function '" <> show fun <> "' is not used. As such, it will be deleted. If the function contains side-effects then this function actually wants to be stateful!"
