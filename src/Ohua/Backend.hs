module Ohua.Backend where

import Ohua.Prelude

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import Ohua.Backend.Communication
import Ohua.Backend.Fusion
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), (</>), takeExtension)


backend :: 
        ( CompM m
        , Integration lang
        , Architecture arch
        , Lang arch ~ lang
        ) 
        => FilePath 
        -> Namespace (TCProgram Channel (Com 'Recv) FusableExpr) 
        -> lang 
        -> arch 
        -> m ()
backend outDir compiled lang arch =
    fuse compiled >>=
    (pure . lowerChannels arch . lowerTaskCom arch) >>=
    Types.lower lang arch >>=
    Types.build arch lang >>=
    Types.serialize arch lang >>=
    mapM_ writeFile
    where
        writeFile (file, code) = do 
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        
