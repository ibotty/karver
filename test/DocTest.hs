{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (handle, SomeException)
import System.Environment (getEnv)
import System.Directory (doesDirectoryExist
                        , getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)


main :: IO ()
main = do
  distDir <- findDistDir
  let defaultOpts = getOptions distDir
  glob "src/**/*.hs" >>= doctest . (defaultOpts ++)
   where
     getOptions distDir = [ "-XCPP"
                          , "-optP-include"
                          , "-optP" ++ distDir ++ "/build/autogen/cabal_macros.h"
                          ]

findDistDir :: IO FilePath
findDistDir = do
  distDir <- isDistDir
  case distDir of
    Just fp -> return fp
    Nothing -> do
      stackDir <- isStackDir
      case stackDir of
        Nothing -> return "dist"
        Just dir -> return dir

isDistDir :: IO (Maybe FilePath)
isDistDir = do
  exists <- doesDirectoryExist "dist"
  if exists then return (Just "dist") else return Nothing

isStackDir :: IO (Maybe FilePath)
isStackDir = handle (\(_ :: SomeException) -> getStackDir) $ do
    stackDir <- getEnv "HASKELL_DIST_DIR"
    return $ Just stackDir

-- More fragile way of getting stack dir
-- relies on there being only one item in dist/ARCH/cabal-vers/
-- necessary for old versions of stack before HASKELL_DIST_DIR
getStackDir :: IO (Maybe FilePath)
getStackDir = do
  let stackDir = ".stack-work/dist"
  exists <- doesDirectoryExist stackDir
  if not exists then return Nothing else do
      stackWithArch <- getSoleItem stackDir
      case stackWithArch of
        Just fpath -> getSoleItem fpath
        Nothing -> return Nothing

getSoleItem :: FilePath -> IO (Maybe FilePath)
getSoleItem fpath = do
  contents <- getDirectoryContents fpath
  let soleValue = filter (\fp -> head fp /= '.') contents
  if null soleValue
    then return Nothing
    else return $ Just (fpath </> head soleValue)
