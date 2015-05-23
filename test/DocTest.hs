module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest . (defaultOpts ++)
   where
     defaultOpts = [ "-XCPP"
                   , "-optP-include"
                   , "-optPdist/build/autogen/cabal_macros.h"
                   ]
