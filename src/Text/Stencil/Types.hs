{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
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
  ( Context(..)
  , Dict
  , List
  , defaultAsBool
  , defaultAsDict
  , asEscapedNoEscape
  , defaultAsList
  , defaultAsText
  , asEscapedWithEscaper
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

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative    ((<$>))
import Data.Monoid            (Monoid, mappend, mempty)
#endif

import Data.HashMap.Strict    (HashMap)
import Data.String            (IsString, fromString)
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable          (Typeable)
import Data.Vector            (Vector, (!?))

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Vector         as V


type List = Vector Value
type Dict = HashMap Text Value

data Context = Context { asEscaped :: Value -> Maybe Text
                       , asText    :: Value -> Maybe Text
                       , asList    :: Value -> Maybe List
                       , asDict    :: Value -> Maybe Dict
                       , asBool    :: Value -> Maybe Bool
                       , dict      :: Dict
                       }

-- that should be more typesafe. see upcoming lambda definition.
-- but what to do with dicts and lists? Tags (a la Typeable)?
data Value = EscapedV Text
           | TextV    Text
           | DictV    Dict
           | ListV    List
           | BoolV    Bool
           | IntV     Int
           -- Lambda   :: (b -> a) -> Value

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

asEscapedWithEscaper :: (Text -> Text) -> Value -> Maybe Text
asEscapedWithEscaper _ (EscapedV t) = Just t
asEscapedWithEscaper escape v       = escape <$> defaultAsText v

asEscapedNoEscape :: Value -> Maybe Text
asEscapedNoEscape = asEscapedWithEscaper id

defaultAsText :: Value -> Maybe Text
defaultAsText (TextV t)     = Just t
defaultAsText (EscapedV t)  = Just t
defaultAsText (IntV i)      = Just . T.pack $ show i
defaultAsText (BoolV True)  = Just "True"
defaultAsText (BoolV False) = Just "False"
defaultAsText _             = Nothing

defaultAsDict :: Value -> Maybe Dict
defaultAsDict (DictV d) = Just d
defaultAsDict (ListV l) =
    Just . HM.fromList $ V.ifoldl' step mempty l
  where
    step xs i x = (T.pack (show i), x) : xs
defaultAsDict _         = Nothing

defaultAsList :: Value -> Maybe List
defaultAsList (ListV l) = Just l
defaultAsList _         = Nothing

defaultAsBool :: Value -> Maybe Bool
defaultAsBool (BoolV b)    = Just b
defaultAsBool (IntV  i)    = Just $ i > 0
defaultAsBool (TextV t)    = Just . not $ T.null t
defaultAsBool (EscapedV t) = Just . not $ T.null t
defaultAsBool (ListV l)    = Just . not $ V.null l
defaultAsBool (DictV d)    = Just . not $ HM.null d

lookupKey :: Context -> Text -> Maybe Value
lookupKey ctx k = HM.lookup k (dict ctx)

lookupVariable :: Variable -> Context -> Maybe Value
lookupVariable (Variable v)    ctx = lookupKey ctx v
lookupVariable (ObjectKey v k) ctx =
    case lookupKey ctx v of
      Just (DictV d) -> HM.lookup k d
      _              -> Nothing
lookupVariable (ListIndex v i) ctx =
    case lookupKey ctx v of
      Just (ListV l) -> l !? i
      _              -> Nothing


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

instance A.FromJSON Value where
    parseJSON o@(A.Object _) = DictV <$> A.parseJSON o
    parseJSON a@(A.Array _)  = ListV <$> A.parseJSON a
    parseJSON v              = TextV . fromString <$> A.parseJSON v
    {-# INLINE parseJSON #-}

instance IsString Value where
    fromString = EscapedV . fromString
