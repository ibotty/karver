{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Karver.Compiler
  ( ResolveIncludes(..)
  , resolveIncludes
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Data.Text (Text)
import Text.Karver.Types
import Text.Karver.Parse
import Data.Attoparsec.Text

import qualified Data.Text as T

newtype ResolvedIncludes repr = ResolvedIncludes repr

instance Monoid repr => Monoid (ResolvedIncludes repr) where
    mempty = ResolvedIncludes mempty
    mappend (ResolvedIncludes a) (ResolvedIncludes a') =
      ResolvedIncludes $ a <> a'

data ResolveIncludes repr = ResolveIncludes (FilePath -> Maybe Text) repr

resolveIncludes :: (IncludeSYM repr, BasicSYM repr') => repr -> Maybe repr'
resolveIncludes = undefined

instance BasicSYM repr => BasicSYM (ResolvedIncludes repr) where
    literal n = literal n
    identity var = identity var
    object v k  = object v k
    list l v = list l v
    condition c t f = condition c t f
    loop l i b = loop l i b

instance (BasicSYM repr, BasicSYM repr') => BasicSYM (ResolveIncludes repr -> Maybe repr') where
    literal n _ = Just $ literal n
    identity var _ = Just $ identity var
    object v k  _ = Just $ object v k
    list l v _ = Just $ list l v
    condition c t f inc = condition c <$> t inc <*> f inc
    loop l i b inc = loop l i <$> b inc

instance (IncludeSYM repr, BasicSYM repr') => IncludeSYM (ResolveIncludes repr -> Maybe repr') where
    include f (ResolveIncludes inc r) = do
      tmpl <- inc f
      case parseOnly templateParser tmpl of
        Right (ResolvedIncludes t) -> Just (t :: repr')
        Left _ -> Nothing

-- pushIncludes :: IncludeSYM repr => (FilePath -> Maybe Text) -> (ResolveIncludes -> Maybe repr) -> Maybe repr
-- pushIncludes inc r = r (ResolveIncludes inc)

-- pushIncludes' inc r = go r
--   where go r' = pushIncludes inc r'
