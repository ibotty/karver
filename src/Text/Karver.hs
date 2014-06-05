{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Data.Karver
-- Copyright:   Jeremy Hull 2013
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- The "Text.Karver" interface for translation 'Text' from it's template
-- syntax, to a generated value — based on the data that was given.

module Text.Karver
( renderTemplate
, renderTemplate'
, unsafeLazyTemplate
, loadTemplates
, module Text.Karver.Types
) where

import Text.Karver.Compiler
import Text.Karver.Parse
import Text.Karver.Types

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Aeson (decode')
import Data.Attoparsec.Text (parseOnly)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import System.Directory (getCurrentDirectory, getDirectoryContents, getPermissions, doesDirectoryExist, doesFileExist, readable)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.IO as TI

-- | Renders a template
renderTemplate :: (FilePath -> Maybe Text)
               -- ^ load templates that are included
               -> (KarverError -> Either KarverError Text)
               -- ^ error handler
               -> HashMap Text Value
               -- ^ Data map for variables inside
               --   a given template
               -> Text
               -- ^ Template
               -> Either KarverError TL.Text
renderTemplate loader handler ctx tmpl =
    case parseOnly (templateParser loader) tmpl of
      Right repl -> TB.toLazyText . rawRenderer <$> renderParsedTemplate handler ctx repl
      Left err -> TL.fromStrict <$> handler (InvalidTemplate "(inline)" err)


unsafeLazyTemplate :: FilePath -> Maybe Text
unsafeLazyTemplate f = unsafePerformIO $
    doesFileExist f >>= \case
      False -> return Nothing
      True  -> try' $ TI.readFile f
  where
    try' :: IO Text -> IO (Maybe Text)
    try' = fmap (either (\(_ :: SomeException) -> Nothing) Just) . try
{-# NOINLINE unsafeLazyTemplate #-}

loadTemplates :: IO (FilePath -> Maybe Text)
loadTemplates = flip H.lookup <$> (go =<< getCurrentDirectory)
  where
    go :: FilePath -> IO (HashMap FilePath Text)
    go f = do
        allFiles <- filterM isReadable =<< getDirectoryContents f
        files <- filterM doesFileExist allFiles
        filesContents <- zip files <$> mapM TI.readFile files
        subdirs  <- filterM doesDirectoryExist allFiles
        let map' = H.fromList filesContents
        H.unions . (map' :) <$> mapM go subdirs

    isReadable :: FilePath -> IO Bool
    isReadable = fmap readable . getPermissions

continueHandler :: KarverError -> Either KarverError Text
continueHandler (InvalidTemplate _ _) = Right T.empty
continueHandler (LookupError _) = Right T.empty
continueHandler (NoSuchInclude err) = Right err
continueHandler (ManyErrors _) = Right T.empty

-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate' :: Text -- ^ JSON data, for variables inside a given
                        --   template
                -> Text -- ^ Template
                -> TL.Text
renderTemplate' json tpl =
  case decode' . L.pack $ T.unpack json of
    (Just hash) -> case renderTemplate unsafeLazyTemplate continueHandler hash tpl of
                     Left err -> error $ "renderTemplate': something went wrong: " ++ show err
                     Right a -> a
    Nothing     -> error "renderTemplate': could not decode JSON."
