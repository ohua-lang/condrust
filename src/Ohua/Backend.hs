module Ohua.Backend where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import Ohua.Backend.Communication as Com
import Ohua.Backend.Fusion as Fusion
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((</>))


backend :: 
        ( CompM m
        , Integration lang
        , Architecture arch
        , Lang arch ~ lang
        , ty ~ Type (Lang arch)
        ) 
        => FilePath 
        -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (FusableExpr ty)) 
        -> NS lang
        -> arch 
        -> m ()
backend outDir compiled lang arch =
    Fusion.fuse compiled >>=
    (pure . Com.intoProgram . Com.lowerChannels arch . Com.lowerTaskCom arch) >>=
    Types.lower lang arch >>=
    Types.build arch lang >>=
    Types.serialize arch lang >>=
    mapM_ writeFile
    where
        writeFile (file, code) = do 
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        
