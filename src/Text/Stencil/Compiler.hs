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

import Data.Monoid            (mconcat)
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
        case asEscaped ctx =<< lookupVariable var ctx of
          Just s -> literal $ TLB.fromText s
          _      -> variable var

    condition c@(VariableNotNull v) t f ctx =
        case asBool ctx =<< lookupVariable v ctx of
          Just True  -> tokens $ map ($ ctx) t
          Just False -> tokens $ map ($ ctx) f
          Nothing    -> condition c (map ($ ctx) t) (map ($ ctx) f)

    loop v ident@(Identifier i) body ctx =
        case asList ctx =<< lookupVariable v ctx of
            Just l -> tokens . V.toList $ V.imap (eachListElem (V.length l)) l
            _      -> loop v ident body ctx
      where
        eachListElem lLength ix val = tokens $ map ($ newCtx) body
          where
            newCtx = ctx { dict = newDict }
            newDict = HM.insert "loop" (DictV $ HM.fromList loopObject)
                    $ HM.insert i val
                    $ dict ctx
            -- TODO: implement cycle, depth, depth0
            loopObject = [ ("index",     IntV (ix + 1))
                         , ("index0",    IntV ix)
                         , ("revindex0", IntV (revIx - 1))
                         , ("revindex",  IntV revIx)
                         , ("length",    IntV lLength)
                         , ("first",     BoolV (ix == 0))
                         , ("last",      BoolV (revIx == 1))
                         ]
            revIx = lLength - ix

render :: Builder -> Builder
render = id

instance JinjaSYM Builder where
    tokens = mconcat
    literal = id
