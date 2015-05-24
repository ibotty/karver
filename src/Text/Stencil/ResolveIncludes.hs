{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Text.Stencil.ResolveIncludes
  ( ResolveIncludes()
  , resolveIncludes
  )
  where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative       (Applicative, pure, (<$>), (<*>))
import Data.Traversable          (traverse)
#endif

import Data.Attoparsec.Text.Lazy (Parser, maybeResult, parse)
import Text.Stencil.Types

import qualified Data.Text.Lazy as TL

data ResolveIncludes m a = ResolveIncludes { unwrapRI :: Loader m -> Parser (ResolveIncludes m a) -> m a }

resolveIncludes :: Loader m -> Parser (ResolveIncludes m r) -> ResolveIncludes m r -> m r
resolveIncludes loader parser r = unwrapRI r loader parser

instance (JinjaSYM r, Functor m, Applicative m) =>
    JinjaSYM (ResolveIncludes m r)
  where
    tokens xs = ResolveIncludes $ \loader parser ->
        tokens <$> traverse (resolveIncludes loader parser) xs
    literal t = ResolveIncludes $ \_ _ ->
        pure $ literal t

instance (JinjaVariableSYM r, Functor m, Applicative m) =>
    JinjaVariableSYM (ResolveIncludes m r)
  where
    variable var = ResolveIncludes $ \_ _ ->
       pure $ variable var
    condition c ifBody elseBody = ResolveIncludes $ \loader parser ->
        condition c <$> traverse (resolveIncludes loader parser) ifBody
                    <*> traverse (resolveIncludes loader parser) elseBody

    loop var identifier body = ResolveIncludes $ \loader parser ->
        loop var identifier <$> traverse (resolveIncludes loader parser) body

instance (JinjaIncludeSYM r, Functor m, Monad m, Applicative m) =>
    JinjaIncludeSYM (ResolveIncludes m r)
  where
    include file = ResolveIncludes $ \loader parser ->
        fmap TL.init <$> loader file >>= \case
            Nothing   -> return $ include file
            Just tmpl -> case maybeParse parser tmpl of
                           Nothing -> return $ include file
                           Just r  -> resolveIncludes loader parser r

maybeParse :: Parser r -> TL.Text -> Maybe r
maybeParse parser = maybeResult . parse parser
