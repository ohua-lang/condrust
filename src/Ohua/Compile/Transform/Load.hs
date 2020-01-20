{-|

Module      : $Header$
Description : Loading algorithms from other namespaces into the current one.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This "transformation" loads algorithms from other modules, imported via import statements,
as top-level algos into the current namespace.
It is only somewhat a transformation because it does alter any expression. But this is only the case,
because we chose to represent the list of top-level algos in terms of a hash map. Normally, all would form
one big expression.

-}

module Ohua.Compile.Transform.Load where

-- | Loads the content of an algo file and parses its contents into a (parser-representation) namespace.
-- To prevent namespace clashes, the algo (foo) is always registered as:
-- > some/other/ns:foo => expr
readAndParse ::
       (MonadLogger m, MonadIO m) => FileRef -> m P.Namespace
readAndParse filename = do
    ns <-
        let strFname = toString filename
         in getParser (toText $ takeExtension strFname) <$>
            liftIO (L.readFile strFname)
    logDebugN $ "Raw parse result for " <> filename
    logDebugN $ "<Pretty printing not implemented for frontend lang yet>" -- quickRender ns
    logDebugN "With annotations:"
    logDebugN $ show $ map (^. P.algoTyAnn) ns ^. P.algos
    pure ns

findSourceFile :: (MonadError Error m, MonadIO m) => NSRef -> m Text
findSourceFile modname = do
    candidates <-
        filterM (liftIO . doesFileExist) $
        map ((asFile Path.<.>) . toString) extensions
    case candidates of
        [] -> throwError $ "No file found for module " <> show modname
        [f] -> pure $ toText f
        files ->
            throwError $
            "Found multiple files matching " <> show modname <> ": " <>
            show files
  where
    asFile = toString $ T.intercalate "/" $ map unwrap $ unwrap modname
    extensions = map (^. _1) definedLangs

