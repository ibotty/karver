{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Karver.Compiler
  ( Renderer(..)
  , Renderer'
  , renderParsedTemplate
  )
  where

import Control.Applicative ((<$>))
import Data.HashMap.Strict (HashMap)
import Data.Monoid (Monoid, (<>), mappend, mconcat, mempty)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Text.Karver.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector as V

type Renderer' = (KarverError -> Either KarverError Text)
               -> HashMap Text Value
               -> Either KarverError Renderer

renderParsedTemplate :: (KarverError -> Either KarverError Text)
                     -> HashMap Text Value
                     -> Renderer'
                     -> Either KarverError Renderer
renderParsedTemplate handler ctx tmpl = tmpl handler ctx

newtype Renderer = Renderer {rawRenderer :: Builder}

instance Monoid Renderer where
    mempty = Renderer mempty
    mappend (Renderer a) (Renderer a') =
      Renderer $ a <> a'

instance Monoid (Either KarverError Renderer) where
    mempty = Right mempty
    mappend (Left err) _ = Left err
    mappend _ (Left err) = Left err
    mappend (Right a) (Right a') = Right (a <> a')

rightRendererFromText :: Text -> Either a Renderer
rightRendererFromText = Right . Renderer . TB.fromText

instance JinjaSYM Renderer' where
    literal n _ _ = rightRendererFromText n

    variable var handler ctx =
        case lookupVariable var ctx of
          Just (Literal s) -> rightRendererFromText s
          _                -> Renderer . TB.fromText <$> handler (LookupError var)

    condition (VariableNotNull v) t f handler ctx =
        case lookupVariable v ctx of
          Just (Literal s) -> whenDo $ not (T.null  s)
          Just (List l)    -> whenDo $ not (V.null  l)
          Just (Object o)  -> whenDo $ not (HM.null o)
          Nothing          -> whenDo =<< not . T.null <$> handler (LookupError v)
      where
        whenDo b = (if b then t else f) handler ctx

    loop v (Identifier i) b handler ctx =
        case lookupVariable v ctx of
          Just (List l) -> mconcat . V.toList $ V.map (\val -> b handler $ HM.insert i val ctx) l
          _             -> Renderer . TB.fromText <$> handler (LookupError v)

    include b = b
