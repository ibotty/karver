{-# LANGUAGE CPP #-}
module Text.Stencil.Context
  ( defaultContext
  , emptyContext
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Text.Stencil.Types

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

defaultContext :: Dict -> Context
defaultContext d = emptyContext { dict = d }

emptyContext :: Context
emptyContext = Context asEscapedNoEscape
                       defaultAsText
                       defaultAsList
                       defaultAsDict
                       defaultAsBool
                       mempty

