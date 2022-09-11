module Ohua.Backend where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Types as Types
import Ohua.Backend.Lang
import Ohua.Backend.Normalize (normalize)
import Ohua.Backend.Communication as Com
import Ohua.Backend.Fusion (fuse, FusableExpr)
import Ohua.Backend.Transform (transformTaskExprs, transformTasks)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((</>))


backend ::
        ( CompM m
        , Integration lang
        , Architecture arch
        , Transform arch
        , Lang arch ~ lang
        , ty ~ Type lang
        , anno ~ AlgoSrc lang
        )
        => FilePath
        -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (FusableExpr ty)) anno ty
        -> HostModule lang
        -> arch
        -- REMINDER: Replace type of placeholder when needed
        -> HostModule lang
        -> m ()
backend outDir compiled lang arch placeholder =
    fuse compiled >>=
    (pure . transformTaskExprs lang arch) >>=
    (pure . normalize) >>=
    (pure . Com.intoProgram) >>=
    Types.lower lang arch >>=
    (pure. transformTasks lang arch) >>=
    (pure . Com.lowerChannels arch . Com.lowerRetCom arch) >>=
    Types.build arch lang >>=
    Types.serialize arch lang placeholder >>=
    mapM_ writeFile -- >>= 
    -- writePlaceholder 
    -- REMINDER: Here I probably just need to write 
    -- the module that placeholder holds the place for
    -- REMINDER: Another option is passing it to the Implementation Backend i.e. 
    -- to build or serialize such that the Inntegration can handle the import 
    where
        writeFile (file, code) = do
            let fullPath = outDir </> file
            liftIO $ L.writeFile fullPath code
            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
        {-writePlaceholder _ = do
            let pString = "Hier koennte Ihre Wernung stehen"
            let path = outDir </> "placeholder_module"
            liftIO $ L.writeFile path pString
            logInfoN $ "Placeholder written to'" <> T.pack path <> "'"-}
            