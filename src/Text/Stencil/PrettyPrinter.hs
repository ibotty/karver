{-# LANGUAGE OverloadedStrings #-}
module Text.Stencil.PrettyPrinter
  ( PrettyPrinter(..)
  , printAST
  ) where

import Data.Monoid       ((<>))
import Data.Text         (Text)
import Text.Stencil.Types

import qualified Data.Text as T

printAST :: PrettyPrinter -> PrettyPrinter
printAST = id

newtype PrettyPrinter = PrettyPrinter Text
  deriving (Eq, Read, Show)

pprint :: Show a => Text -> a -> PrettyPrinter
pprint label t = PrettyPrinter $ label <> " " <> T.pack (show t)

instance JinjaSYM PrettyPrinter where
    tokens = pprint "tokens "
    literal = pprint "literal"
    variable = pprint "variable"
    condition c t f = pprint "condition" (c, t, f)
    loop l i b = pprint "loop" (l, i, b)

instance JinjaIncludeSYM PrettyPrinter where
    include = pprint "include"

