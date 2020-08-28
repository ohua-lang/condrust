module Ohua.Backend where

import Ohua.Prelude

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), (</>), takeExtension)


backend :: 
        ( CompM m
        , Integration lang
        , Architecture arch
        , lang ~ Integ arch
        ) 
        => FilePath -> Namespace (TCProgram Channel TaskExpr) -> lang -> arch -> m ()
backend outDir compiled lang arch =
    Types.lower lang compiled >>=
    Types.build arch >>=
    Types.serialize arch >>=
    mapM_ writeFile
    where
        writeFile (file, code) = do 
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        
