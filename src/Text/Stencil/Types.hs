{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:      Data.Stencil.Types
-- Copyright:   Tobias Florek 2014
-- License:     BSD3
--
-- Maintainer:  Tobias Florek <tob@butter.sh>
-- Stability:   experimental
-- Portability: unknown
--
-- Base types used throughout Stencil.

module Text.Stencil.Types
  ( Context
  , Value(..)
  , StencilError(..)
  , ErrorHandler
  , Loader
  , JinjaSYM(..)
  , JinjaVariableSYM(..)
  , JinjaIncludeSYM(..)
  , Variable(..)
  , Key(..)
  , Condition(..)
  , Identifier(..)
  , duplicate
  , lookupVariable
  ) where

import Control.Applicative ((<$>))
import Data.HashMap.Strict (HashMap)
import Data.Monoid         (Monoid, mappend, mempty)
import Data.String         (IsString, fromString)
import Data.Text           (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable       (Typeable)
import Data.Vector         (Vector, (!?))

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

type Context = HashMap Text Value

data StencilError = InvalidTemplate Text String
                  | InvalidTemplateFile FilePath
                  | NoSuchInclude FilePath
                  | LookupError Variable
                  | ManyErrors [StencilError]
  deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid StencilError where
  mappend e e' = ManyErrors [e, e']
  mempty = ManyErrors []

type ErrorHandler r = StencilError -> Either StencilError r

type Loader m = FilePath -> m (Maybe TL.Text)

data Variable = Variable Text
              | ObjectKey Text Text
              | ListIndex Text Int
  deriving (Eq, Ord, Read, Show)

lookupVariable :: Variable -> Context -> Maybe Value
lookupVariable (Variable v) ctx = HM.lookup v ctx
lookupVariable (ObjectKey v k) ctx =
    case HM.lookup v ctx of
      Just (Object o) -> HM.lookup k o
      _               -> Nothing
lookupVariable (ListIndex v i) ctx =
    case HM.lookup v ctx of
      Just (List l) -> l !? i
      _             -> Nothing


newtype Key = Key Text
  deriving (Eq, Ord, Read, Show)

-- | XXX should have other conditions
data Condition = VariableNotNull Variable -- ^ {% if name %}
               -- | VariableDefined Variable -- ^ {% if name is defined %}
  deriving (Eq, Ord, Read, Show)

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Read, Show)

class JinjaSYM repr where
    tokens :: [repr] -> repr
    literal :: Builder -> repr

class JinjaVariableSYM repr where
    variable :: Variable -> repr
    condition :: Condition -> [repr] -> [repr] -> repr
    loop :: Variable -> Identifier -> [repr] -> repr

class JinjaIncludeSYM repr where
    include :: FilePath -> repr

instance (JinjaSYM repr, JinjaSYM repr') => JinjaSYM (repr, repr') where
    tokens terms = (tokens xs, tokens xs')
      where (xs, xs') = unzip terms
    literal x = (literal x, literal x)

instance (JinjaVariableSYM repr, JinjaVariableSYM repr') => JinjaVariableSYM (repr, repr') where
    variable var = (variable var, variable var)
    condition l t f = (condition l ifB elseB, condition l ifB' elseB')
      where (ifB, ifB') = unzip t
            (elseB, elseB') = unzip f
    loop l ident b = (loop l ident body, loop l ident body')
      where (body, body') = unzip b

instance (JinjaIncludeSYM repr, JinjaIncludeSYM repr') => JinjaIncludeSYM (repr, repr') where
    include path = (include path, include path)

duplicate :: (repr, repr') -> (repr, repr')
duplicate = id

-- | Fairly basic work around for using different types inside a 'HashMap'.
-- The 'Value' type also make it possible for 'List' to contain more than
-- one type.
data Value = Literal Builder
           -- ^ The base value for the storing of variable.
           | Object Context
           -- ^ An alias for 'Context', that will only hold 'Text' with
           --   'Text' as a key as well.
           | List   (Vector Value)
           -- ^ An alias for 'Vector', that can hold all three 'Value's
           --   — which isn't desirable, because their can be nested
           --   'List's.
           deriving (Show, Eq)

instance A.FromJSON Value where
  parseJSON o@(A.Object _) = Object <$> A.parseJSON o
  parseJSON a@(A.Array _)  = List <$> A.parseJSON a
  parseJSON v              = Literal . TLB.fromText <$> A.parseJSON v
  {-# INLINE parseJSON #-}

instance IsString Value where
  fromString = Literal . TLB.fromString
