{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import Test.Tasty.Golden (goldenVsString)
import Text.Stencil
import Text.Stencil.Config
import System.Directory (getDirectoryContents)

import qualified Test.Tasty as Tasty
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE

main :: IO ()
main =
    Tasty.defaultMain =<< setupGoldenTests "golden"

setupGoldenTests :: FilePath -> IO Tasty.TestTree
setupGoldenTests dir = do
    ctx <- fromMaybe (error "cannot decode context.json") . A.decode'
           <$> BL.readFile (testDir <> "/context.json")
    let run f = fmap (either renderFailure TLE.encodeUtf8)
              . renderTemplate config ctx
              =<<  TLE.decodeUtf8 <$> BL.readFile f
        testFromFile f = goldenVsString f (f <> ".golden") (run f)

    goldenInputs <- map (testDir <>) . filter (isSuffixOf ".jinja")
                <$> getDirectoryContents testDir
    let goldenTests = map testFromFile goldenInputs

    print goldenInputs

    return $ Tasty.testGroup "Golden tests in test/golden" goldenTests
  where
    testDir = "test/" <> dir <> "/"
    config = setLoader loadTemplates defaultConfig
    renderFailure = error . ("cannot render file " <>) . show

-- tests :: Tasty.TestTree
-- tests = Tasty.testGroup "Tests" [unitTests]

-- unitTests :: Tasty.TestTree
-- unitTests = Tasty.testGroup "Unit tests" []