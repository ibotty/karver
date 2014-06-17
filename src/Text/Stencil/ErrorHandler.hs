{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Stencil.ErrorHandler
  ( ErrorHandler'
  , IgnoreHandler(..)
  , FailHandler(..)
  , FailIncludesHandler(..)
  -- , handleError
  -- , noHandleHandler
  -- , continueHandler
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Traversable (traverse)
import Data.Monoid (mempty)
import Text.Stencil.Types

type ErrorHandler' r = ErrorHandler r -> Either StencilError r

newtype FailHandler r = FailHandler { failHandler :: Either StencilError r}
newtype FailIncludesHandler r = FailIncludesHandler { failIncludesHandler :: Either StencilError r}
newtype IgnoreHandler r = IgnoreHandler { ignoreHandler :: r }

-- | Use 'ErrorHandler' to loop through the representation possibly resolving
-- any not bound 'variable' and 'include'.
-- handleError :: ErrorHandler' r -> Either StencilError r
-- handleError handler r = r handler

-- ignoreHandler :: IgnoreHandler r -> r
-- IgnoreHandler = ignoreHandler

noHandleHandler :: ErrorHandler r
noHandleHandler = Left

continueHandler :: JinjaSYM r => ErrorHandler r
continueHandler = const . Right $ literal mempty

instance JinjaSYM r => JinjaSYM (IgnoreHandler r) where
    tokens = IgnoreHandler . tokens . map ignoreHandler
    literal = IgnoreHandler . literal

instance JinjaSYM r => JinjaIncludeSYM (IgnoreHandler r) where
    include _ = literal mempty

instance JinjaSYM r => JinjaVariableSYM (IgnoreHandler r) where
    variable _ = literal mempty
    condition _ _ _ = literal mempty
    loop _ _ _ = literal mempty

instance JinjaSYM r => JinjaSYM (FailHandler r) where
    tokens = FailHandler . fmap tokens . sequence . map failHandler
    literal = FailHandler . Right . literal

instance JinjaVariableSYM (FailHandler r) where
    variable = FailHandler . Left . LookupError
    condition (VariableNotNull v) _ _ = FailHandler . Left $ LookupError v
    loop v _ _ = FailHandler . Left $ LookupError v

instance JinjaIncludeSYM (FailHandler r) where
    include = FailHandler . Left . NoSuchInclude


instance JinjaSYM r => JinjaSYM (FailIncludesHandler r) where
    tokens = FailIncludesHandler . fmap tokens . sequence . map failIncludesHandler
    literal = FailIncludesHandler . Right . literal

instance JinjaVariableSYM r => JinjaVariableSYM (FailIncludesHandler r) where
    variable = FailIncludesHandler . Right . variable
    condition v i e = FailIncludesHandler $
        condition v <$> sequence (map failIncludesHandler i)
                    <*> sequence (map failIncludesHandler e)
    loop v e b = FailIncludesHandler $
        loop v e <$> sequence (map failIncludesHandler b)

instance JinjaIncludeSYM (FailIncludesHandler r) where
    include = FailIncludesHandler . Left . NoSuchInclude


instance JinjaIncludeSYM (ErrorHandler' r) where
    include file handler = handler $ InvalidTemplateFile file

instance JinjaSYM r => JinjaSYM (ErrorHandler' r) where
    tokens xs handler = tokens <$> traverse ($ handler) xs
    literal t _ = Right $ literal t

instance JinjaVariableSYM r => JinjaVariableSYM (ErrorHandler' r) where
    variable var handler = handler $ LookupError var
    condition c b b' handler = condition c <$> traverse ($ handler) b
                                           <*> traverse ($ handler) b'
    loop var ident xs handler = loop var ident <$> traverse ($ handler) xs

