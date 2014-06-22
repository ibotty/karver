module Text.Stencil.Context
  ( defaultContext
  , emptyContext
  ) where

import Text.Stencil.Types

import Data.Monoid (mempty)

defaultContext :: Dict -> Context
defaultContext d = emptyContext { dict = d }

emptyContext :: Context
emptyContext = Context asEscapedNoEscape
                       defaultAsText
                       defaultAsList
                       defaultAsDict
                       defaultAsBool
                       mempty

