{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Stencil.ResolveIncludes
  ( ResolveIncludes
  , resolveIncludes
  )
  where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Attoparsec.Text.Lazy (Parser, parse, eitherResult)
import Text.Stencil.Parse
import Text.Stencil.Types

import qualified Data.Text.Lazy as TL

type ResolveIncludes m a = Loader m -> m (Either StencilError a)

resolveIncludes :: (JinjaSYM repl, Monad m) => Loader m -> ResolveIncludes m repl -> m (Either StencilError repl)
resolveIncludes loader repr = repr loader

right :: Monad m => b -> m (Either a b)
right = return . Right

left :: Monad m => a -> m (Either a b)
left = return . Left

instance (JinjaSYM repr, Functor m, Monad m) => JinjaSYM (ResolveIncludes m repr) where
    tokens xs loader = runExceptT $ tokens <$> mapM (ExceptT . ($ loader)) xs
    literal = const . right . literal
    variable = const . right . variable
    condition c ifBody elseBody loader = runExceptT $
        condition c <$> mapM (ExceptT . ($ loader)) ifBody
                    <*> mapM (ExceptT . ($ loader)) elseBody

    loop var identifier body loader = runExceptT $
        loop var identifier <$> mapM (ExceptT . ($ loader)) body

instance (JinjaSYM repl, Functor m, Monad m) =>
    JinjaIncludeSYM (Loader m -> m (Either StencilError repl)) where
      include file loader = fmap TL.init <$> loader file >>=
          maybe (left $ NoSuchInclude file)
                (either left ($ loader) . eitherParse file templateParser)

eitherParse :: FilePath -> Parser r -> TL.Text -> Either StencilError r
eitherParse file parser =
      either (Left . InvalidTemplateFile file) Right
    . eitherResult
    . parse parser
