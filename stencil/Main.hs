{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>), (<|>), many)
import Data.Aeson (decode')
import Data.Monoid ((<>))
import GHC.IO.Handle (Handle)
import GHC.IO.Handle.FD (stdin, stdout, openFile)
import Options.Applicative (Parser, str, metavar, argument, execParser, strOption, short, long, help, value, info, helper, progDesc, header, fullDesc)
import Text.Stencil (Context, renderTemplateFile, defaultConfig)
import Text.Stencil.Types (Value(Literal))
import System.IO (IOMode(ReadMode, WriteMode), hSetBinaryMode)
import System.Exit (exitFailure, exitSuccess)

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.Builder as TLB

data FileOrHandle = File    FilePath
                  | Handle' Handle

data Opts = Opts { context      :: Either FileOrHandle Context
                 , outputHandle :: FileOrHandle
                 , template     :: FilePath
                 }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseContextOpt
                 <*> parseOutputOpt
                 <*> parseTemplateOpt

parseTemplateOpt :: Parser FilePath
parseTemplateOpt = argument str ( metavar "TEMPLATE" )

parseOutputOpt :: Parser FileOrHandle
parseOutputOpt =
    fileOrHandle stdout <$>
         strOption
             (  short 'o'
             <> long "output"
             <> help "output file. '-' for stdout."
             <> metavar "OUTPUT"
             <> value "-"
             )


parseContextOpt :: Parser (Either FileOrHandle Context)
parseContextOpt =
    (Left . fileOrHandle stdin <$>
         strOption
            (  short 'i'
            <> long "vars-file"
            <> help "Context as JSON document in specified file. '-' for stdin."
            <> metavar "VARSFILE"
            )
    ) <|>
    (Right . HM.fromList . map (second (Literal . TLB.fromText . T.tail) . T.breakOn "=" . T.pack) <$> many (
         strOption
             (  short 'v'
             <> long "var"
             <> help "variables (in 'name=value' format)."
             )
         )
    )

fileOrHandle :: Handle -> String -> FileOrHandle
fileOrHandle h "-" = Handle' h
fileOrHandle _ f = File f

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOpts)
               (  fullDesc
               <> progDesc "Render a stencil template."
               <> header "stencil - template engine"
               )

run :: Opts -> IO ()
run opts = do
    ctx <- case context opts of
             Left (Handle' h) -> parseContext h
             Left (File f)    -> parseContext =<< openFile f ReadMode
             Right c          -> return c
    outH <- case outputHandle opts of
              Handle' h -> return h
              File f -> openFile f WriteMode
    hSetBinaryMode outH True

    eTmpl <- renderTemplateFile stencilConf ctx (template opts)
    case eTmpl of
      Right tmpl -> BL.hPut outH $ TLE.encodeUtf8 tmpl
      Left err -> do
          putStrLn $ "Error: could not render template: " <> show err
          exitFailure
    exitSuccess
  where
    stencilConf = defaultConfig

    parseContext :: Handle -> IO Context
    parseContext h = do
      hSetBinaryMode h True
      maybeCtx <- decode' <$> BL.hGetContents h
      case maybeCtx of
        Nothing -> putStrLn "Error: cannot decode json." >> exitFailure
        Just c -> return c

