{-|

Module      : $Header$
Description : Utility functions for the ohuac compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}

module Ohua.Compile.Util where

import Ohua.Prelude

import Ohua.Compile.Types

import qualified Data.Text as T (unpack, pack, intercalate)
import System.FilePath.Posix (pathSeparator, addExtension)


nsToFilePath :: NSRef -> FilePath
nsToFilePath = T.unpack . (T.intercalate $ T.pack [pathSeparator]) . (map unwrap) . unwrap 

toFilePath :: (NSRef, Text) -> FilePath
toFilePath (nsRef, suffix) = addExtension (nsToFilePath nsRef) $ T.unpack suffix

runExceptM :: ExceptT Error IO a -> IO a
runExceptM c = runExceptT c >>= either error pure
  
runCompM :: LogLevel -> ExceptT Error (LoggingT IO) a -> IO a
runCompM targetLevel c =
    runStderrLoggingT $
    filterLogger (\_ level -> level >= targetLevel) $
    runExceptT c >>= either exitError pure
  where
    exitError message = do
        logErrorN message
        exitFailure
