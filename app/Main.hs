{-# LANGUAGE TemplateHaskell, CPP #-}
module Main where

import qualified Data.String as Str
import Data.Time (getCurrentTime)
import Language.Haskell.TH (Exp (LitE), Lit (StringL))
import Ohua.Compile.Compiler (compile, langInfo)
import Ohua.Compile.Config
import Ohua.Integration.Lang
import Ohua.Commons.Prelude
import Options.Applicative as O
import Options.Applicative.Help.Pretty as O

data Command
  = Build CommonCmdOpts
  | ShowVersion
  | Language Text

data CommonCmdOpts = CommonCmdOpts
  { inputModuleFile :: FilePath,
    outputPath :: FilePath,
    config :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser odef
  case opts of
    ShowVersion -> do
      putStrLn ("ohuac v" <> CURRENT_PACKAGE_VERSION :: Text)
      putStrLn
        ( "Compiled at "
            <> $(LitE . StringL . show <$> liftIO getCurrentTime) ::
            Text
        )
    Build CommonCmdOpts {..} -> do
      copts@CompilerOptions {..} <- runExceptM $ loadConfig config
      let coreOpts = extractCoreOptions copts
      let backendConf = extractBackendConfig copts
      let targetConf = extractIntegrationConfig copts
      runErrAndLogM
        (logLevel debug)
        $ compile inputModuleFile compilationScope coreOpts backendConf targetConf outputPath
    Language lang -> langInfo lang
  where
    odef =
      info
        (helper <*> optsParser)
        ( fullDesc <> header "ohuac ~ the ohua standalone compiler"
            <> progDescDoc
              ( Just $
                  softStr
                    "Compiles algorithm source files into a dataflow program in a particular backend."
                    <$$> "Supported language integrations are:"
                    </> fillSep
                      ( punctuate comma $
                          map (squotes . text . show) [(minBound :: Lang) ..]
                      )
              )
        )
    softStr = fillSep . map text . Str.words
    optsParser =
      hsubparser
        ( command
            "build"
            ( info
                (Build <$> commonOptsParser)
                ( progDescDoc $
                    Just $
                      "Build the ohua graph. " <$$> ""
                )
            )
            <> command
              "version"
              ( info
                  (pure ShowVersion)
                  ( progDesc
                      "Show information, such as the version, about this binary."
                  )
              )
            <> command
              "lang"
              ( info
                  (Language <$> commonLangParser)
                  ( progDesc
                      "Show specific information for a dedicated integration, such as `rust`."
                  )
              )

        )
    commonLangParser = argument str (metavar "LANGUAGE" <> help "Host language")

    commonOptsParser =
      CommonCmdOpts
        <$> argument str (metavar "SOURCE" <> help "Source file to compile")
        <*> strOption
          ( long "output" <> metavar "PATH" <> short 'o'
              <> help "Path to write the output to."
          )
        <*> optional
          ( strOption
              ( long "config" <> metavar "PATH" <> short 'c'
                  <> help "Path to the configuration file for the compilation."
              )
          )
