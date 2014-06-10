{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Text.Stencil.Compiler
  ( Renderer(..)
  , Renderer'
  , renderParsedTemplate
  )
  where

import Control.Applicative    ((<$>))
import Control.Monad          (foldM)
import Data.HashMap.Strict    (HashMap)
import Data.Monoid            (Monoid, mconcat, mempty, (<>))
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)
import Text.Stencil.Types

import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector            as V

type Renderer' = (StencilError -> Either StencilError Text)
               -> HashMap Text Value
               -> Either StencilError Renderer

renderParsedTemplate :: (StencilError -> Either StencilError Text)
                     -> HashMap Text Value
                     -> Renderer'
                     -> Either StencilError Renderer
renderParsedTemplate handler ctx tmpl = tmpl handler ctx

newtype Renderer = Renderer {rawRenderer :: Builder}
  deriving Monoid

rightRendererFromText :: Text -> Either a Renderer
rightRendererFromText = Right . Renderer . TB.fromText

instance JinjaSYM Renderer' where
    tokens xs handler ctx = foldM step mempty xs
      where
        step text b = (text <>) <$> b handler ctx

    literal n _ _ = rightRendererFromText n

    variable var handler ctx =
        case lookupVariable var ctx of
          Just (Literal s) -> rightRendererFromText s
          _                -> Renderer . TB.fromText <$> handler (LookupError var)

    condition (VariableNotNull v) t f handler ctx = mconcat <$>
        case lookupVariable v ctx of
          Just (Literal s) -> whenDo $ not (T.null  s)
          Just (List l)    -> whenDo $ not (V.null  l)
          Just (Object o)  -> whenDo $ not (HM.null o)
          Nothing          -> whenDo =<< not . T.null <$> handler (LookupError v)
      where
        whenDo b = mapM (\i -> i handler ctx) (if b then t else f)

    loop v (Identifier i) body handler ctx =
        case lookupVariable v ctx of
            Just (List l) -> V.foldM' step mempty l
            _             -> handleError
      where
        step :: Renderer -> Value -> Either StencilError Renderer
        step text val =
            (text <>) <$> foldM innerStep mempty body
          where
            innerStep :: Renderer -> Renderer' -> Either StencilError Renderer
            innerStep t r = (t <>) <$> r handler newCtx
            newCtx = HM.insert i val ctx

        handleError = Renderer . TB.fromText <$> handler (LookupError v)
