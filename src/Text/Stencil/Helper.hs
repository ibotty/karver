{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Stencil.Helper
  ( loadTemplatesInDir
  , loadTemplates
  ) where

import Text.Stencil.Types

import Control.Exception (SomeException, try)
import System.Directory     (doesFileExist, getCurrentDirectory)
import System.FilePath      (normalise, (</>))

import qualified Data.Text.Lazy.IO as TLIO

loadTemplatesInDir :: FilePath -> Loader IO
loadTemplatesInDir basePath f =
    doesFileExist file >>= \case
      False -> return Nothing
      True  -> try' $ TLIO.readFile file
  where
    file = normalise $ basePath </> f
    try' = fmap (either (\(_ :: SomeException) -> Nothing) Just) . try

loadTemplates :: Loader IO
loadTemplates file = getCurrentDirectory >>= flip loadTemplatesInDir file
