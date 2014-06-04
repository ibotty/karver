{-# LANGUAGE OverloadedStrings #-}
module Text.Karver.PrettyPrinter
  ( PrettyPrinter(..)
  , printAST
  ) where

import Data.Monoid (Monoid, mempty, mappend, (<>))
import Data.Text (Text)
import Text.Karver.Types

import qualified Data.Text as T

printAST :: PrettyPrinter -> PrettyPrinter
printAST = id

newtype PrettyPrinter = PrettyPrinter Text
  deriving (Eq, Read, Show)

instance Monoid PrettyPrinter where
    mempty = PrettyPrinter mempty
    mappend (PrettyPrinter a) (PrettyPrinter a') =
      PrettyPrinter $ a <> "/n" <> a'

pprint :: Show a => Text -> a -> PrettyPrinter
pprint label t = PrettyPrinter $ label <> " " <> T.pack (show t)

instance JinjaSYM PrettyPrinter where
    literal = pprint "literal"
    variable = pprint "variable"
    condition c t f = pprint "condition" (c, t, f)
    loop l i b = pprint "loop" (l, i, b)
    include = pprint "include"

