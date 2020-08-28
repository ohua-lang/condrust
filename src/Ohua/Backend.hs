module Ohua.Backend where

import Ohua.Prelude

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), (</>), takeExtension)


backend :: (CompM m, Integration lang) => FilePath -> Namespace TCProgram -> lang -> m ()
backend outDir compiled lang = 
    mapM_ writeFile =<< Types.backend compiled lang
    where
        writeFile (file, code) = do 
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        
