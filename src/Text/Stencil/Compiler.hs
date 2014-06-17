{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Stencil.Compiler
  ( bindVariables
  , BindVariables'
  , render
  )
  where

import Data.Monoid            (mconcat, mempty)
import Data.Text.Lazy.Builder (Builder)
import Text.Stencil.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

type BindVariables' r = Context -> r

bindVariables :: Context -> BindVariables' r -> r
bindVariables ctx tmpl = tmpl ctx


instance JinjaSYM r => JinjaSYM (Context -> r) where
    tokens xs ctx = tokens $ map ($ ctx) xs
    literal t _ = literal t

instance (JinjaSYM r, JinjaVariableSYM r) => JinjaVariableSYM (Context -> r)
  where
    variable var ctx =
        case lookupVariable var ctx of
          Just (Literal s) -> literal s
          _                -> variable var

    condition c@(VariableNotNull v) t f ctx =
        case lookupVariable v ctx of
          Just (Literal s) -> whenDo $ s /= mempty
          Just (List l)    -> whenDo $ not (V.null  l)
          Just (Object o)  -> whenDo $ not (HM.null o)
          Nothing          -> condition c (map ($ ctx) t) (map ($ ctx) f)
      where
        whenDo b = tokens $ map ($ ctx) (if b then t else f)

    loop v ident@(Identifier i) body ctx =
        case lookupVariable v ctx of
            Just (List l) -> tokens . V.toList $ V.map eachListElem l
            _             -> loop v ident body ctx
      where
        eachListElem val = tokens $ map ($ newCtx) body
          where
            newCtx = HM.insert i val ctx

render :: Builder -> Builder
render = id

instance JinjaSYM Builder where
    tokens = mconcat
    literal = id
