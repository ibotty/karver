{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Stencil
-- Copyright:   Tobias Florek 2014
-- License:     BSD3
--
-- Maintainer:  Tobias Florek <tob@butter.sh>
-- Stability:   experimental
-- Portability: unknown
--
-- The "Text.Stencil" interface for translation 'Text' from it's template
-- syntax, to a generated value — based on the data that was given.

module Text.Stencil
( renderTemplate
, renderTemplateFile
, renderTemplate'
, renderTemplate''
, loadTemplatesInDir
, loadTemplates
, defaultConfig
, defaultContext
, module Text.Stencil.Types
) where

import Text.Stencil.Compiler
import Text.Stencil.Config
import Text.Stencil.Context
import Text.Stencil.ErrorHandler
import Text.Stencil.Helper
import Text.Stencil.Parse
import Text.Stencil.ResolveIncludes
import Text.Stencil.Types

import Control.Applicative       (Applicative, many, (<$>))
import Data.Aeson                (decode')
import Data.Attoparsec.Text.Lazy (eitherResult, parse)

import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

{- | Render a template using given 'Config'.

>>> let ctx = HM.fromList [("key", Literal "value")]
>>> let tmpl = "{% if key %}the value of key is {{ key }}.{% endif %}"
>>> renderTemplate defaultConfig ctx tmpl
Right "the value of key is value."
-}
renderTemplate :: (Functor m, Applicative m, Monad m)
               => Config m r
               -- ^ Configuration
               -> Context
               -- ^ Data map for variables inside
               --   a given template
               -> TL.Text
               -- ^ Template
               -> m (Either StencilError TL.Text)
renderTemplate config ctx tmpl =
    case eitherResult $ parse (many parser) tmpl of
      Right r -> do
          eResolved <- failIncludesHandler <$> resolveIncludes loader' parser (tokens r)
          return $ do
            resolved <- eResolved
            boundTemplate <- failHandler $ bindVariables ctx resolved
            Right . TLB.toLazyText . render $ boundTemplate
      Left err -> return . Left $ InvalidTemplate "(inline)" err
  where
    parser = templateParser
    loader' = get loader config

renderTemplateFile
  :: (Functor m, Applicative m, Monad m)
  => Config m r -- ^ Configuration
  -> Context    -- ^ Context
  -> FilePath -- ^ template to load using 'Config''s 'Loader'
  -> m (Either StencilError TL.Text)
renderTemplateFile conf ctx file = get loader conf file >>=
    maybe (return . Left $ NoSuchInclude file)
          (renderTemplate conf ctx)

-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate' :: FilePath -- ^ file with JSON data, for variables inside a given
                        --   template
                -> TL.Text -- ^ Template
                -> IO TL.Text
renderTemplate' file tpl =
  decode' <$> BL.readFile file >>= \case
    (Just ctx) -> either err id <$>
        renderTemplate config (defaultContext ctx) tpl
    Nothing     -> error "renderTemplate': could not decode JSON."
  where err e = error $ "renderTemplate': something went wrong: " ++ show e
        config = defaultConfig -- set errorHandler continueHandler defaultConfig



-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate'' :: BL.ByteString -- ^ file with JSON data, for variables inside a given
                        --   template
                -> TL.Text -- ^ Template
                -> IO TL.Text
renderTemplate'' json tpl =
  case decode' json of
    (Just ctx) -> either err id <$>
        renderTemplate config (defaultContext ctx) tpl
    Nothing     -> error "renderTemplate': could not decode JSON."
  where err e = error $ "renderTemplate': something went wrong: " ++ show e
        config = defaultConfig -- set errorHandler continueHandler defaultConfig

-- $setup
-- This is the doctest setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoMonomorphismRestriction
-- >>> import qualified Data.HashMap.Strict as HM
