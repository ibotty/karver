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

import qualified Data.HashMap.Strict    as HM
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector            as V

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
            Just (List l) -> tokens . V.toList $ V.imap (eachListElem (V.length l)) l
            _             -> loop v ident body ctx
      where
        eachListElem lLength ix val = tokens $ map ($ newCtx) body
          where
            newCtx = HM.insert "loop" (Object $ HM.fromList loopObject)
                   $ HM.insert i val
                     ctx
            -- TODO: revisit first, last when proper boolean handling happens
            -- TODO: implement cycle, depth, depth0
            loopObject = [ ("index", show' (ix + 1))
                         , ("index0", show' ix)
                         , ("revindex0", show' (revIx - 1))
                         , ("revindex", show' revIx)
                         , ("length", show' lLength)
                         , ("first", if ix == 0 then "True" else "" )
                         , ("last", if revIx == 1 then "True" else "" )
                         ]
            revIx = lLength - ix
            show' = Literal . TLB.fromString . show

render :: Builder -> Builder
render = id

instance JinjaSYM Builder where
    tokens = mconcat
    literal = id
