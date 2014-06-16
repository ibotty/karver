{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
, continueHandler
, defaultConfig
, module Text.Stencil.Types
) where

import Text.Stencil.Config
import Text.Stencil.Compiler
import Text.Stencil.Helper
import Text.Stencil.Parse
import Text.Stencil.ResolveIncludes
import Text.Stencil.Types

import Control.Applicative  (many, (<$>))
import Data.Aeson           (decode')
import Data.Attoparsec.Text.Lazy (eitherResult, parse)
import Data.HashMap.Strict  (HashMap)
import Data.Text            (Text)

import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB

type Context = HashMap Text Value

{- | Render a template using given 'Config'.

>>> ctx = HM.fromList [("key", "value")]
>>> renderTemplate defaultConfig ctx "{% if key %} the value of key is {{ value }} {% endif %}"
-}
renderTemplate :: (Functor m, Monad m)
               => Config m
               -- ^ Configuration
               -> Context
               -- ^ Data map for variables inside
               --   a given template
               -> TL.Text
               -- ^ Template
               -> m (Either StencilError TL.Text)
renderTemplate config ctx tmpl =
    case eitherResult $ parse (many templateParser) tmpl of
      Right r -> do
          eResolved <- resolveIncludes loader' (tokens r)
          return $ case eResolved of
            Left err -> TL.fromStrict <$> handler err
            Right resolved ->
                TB.toLazyText . rawRenderer <$> renderParsedTemplate handler ctx resolved
      Left err -> return $ TL.fromStrict <$> handler (InvalidTemplate "(inline)" err)
  where
    handler = confErrorHandler config
    loader' = confLoader config

renderTemplateFile
  :: (Functor m, Monad m)
  => Config m -- ^ Configuration
  -> Context  -- ^ Context
  -> FilePath -- ^ template to load using 'Config''s 'Loader'
  -> m (Either StencilError TL.Text)
renderTemplateFile conf ctx file = confLoader conf file >>=
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
        renderTemplate config ctx tpl
    Nothing     -> error "renderTemplate': could not decode JSON."
  where err e = error $ "renderTemplate': something went wrong: " ++ show e
        config = setLoader loadTemplates
               $ setErrorHandler continueHandler
                 defaultConfig



-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate'' :: BL.ByteString -- ^ file with JSON data, for variables inside a given
                        --   template
                -> TL.Text -- ^ Template
                -> IO TL.Text
renderTemplate'' json tpl =
  case decode' json of
    (Just ctx) -> either err id <$>
        renderTemplate config ctx tpl
    Nothing     -> error "renderTemplate': could not decode JSON."
  where err e = error $ "renderTemplate': something went wrong: " ++ show e
        config = setLoader loadTemplates
               $ setErrorHandler continueHandler
                 defaultConfig

