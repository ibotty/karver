{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Stencil.Helper
  ( loadTemplatesInDir
  , loadTemplates
  , continueHandler
  , noHandleHandler
  ) where

import Text.Stencil.Types

import Control.Exception (SomeException, try)
import System.Directory     (doesFileExist, getCurrentDirectory)
import System.FilePath      (normalise, (</>))

import qualified Data.Text as T
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

noHandleHandler :: ErrorHandler
noHandleHandler = Left

continueHandler :: ErrorHandler
continueHandler (InvalidTemplate _ _) = Right T.empty
continueHandler (InvalidTemplateFile _ _) = Right T.empty
continueHandler (LookupError _) = Right T.empty
continueHandler (NoSuchInclude err) = Right (T.pack err)
continueHandler (ManyErrors _) = Right T.empty

