module Ohua.Backend where

import Ohua.Prelude

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), (</>), takeExtension)


backend :: 
        ( CompM m
        , Integration integ
        , Architecture arch
        , integ ~ Integ arch
        ) 
        => FilePath 
        -> Namespace (TCProgram Channel TaskExpr) -> integ -> arch -> m ()
backend outDir compiled integ arch =
    Types.lower integ compiled >>=
    Types.build arch integ >>=
    Types.serialize arch integ >>=
    mapM_ writeFile
    where
        writeFile (file, code) = do 
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        
