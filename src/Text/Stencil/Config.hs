{-# LANGUAGE RankNTypes #-}
module Text.Stencil.Config
  ( Config()
  , Extension(..)
  , defaultConfig
  , minimalConfig
  , loader
  , extensions
  , errorHandler
  , addExtension
  , Lens'
  , set
  , get
  , over
  ) where

import Text.Stencil.Helper
import Text.Stencil.Types

import Data.Functor.Identity (Identity(Identity), runIdentity)
import Control.Applicative (Const(Const), getConst)
import Data.Set (Set)

import qualified Data.Set as Set

data Extension = LoadIncludes
  deriving (Eq, Show, Read, Ord, Bounded)

data Config m r = Config
  { confLoader :: Loader m
  , confExtensions :: Set Extension
  , confErrorHandler :: ErrorHandler r
  }

-- | A 'Config'uration that augments 'minimalConfig' with a
defaultConfig :: Config IO r
defaultConfig = set loader loadTemplates
              $ addExtension LoadIncludes
                minimalConfig

-- | A minimal 'Config'uration so that no includes get loaded.
minimalConfig :: Monad m => Config m r
minimalConfig = Config { confLoader = return . const Nothing
                       , confExtensions = Set.empty
                       , confErrorHandler = Left
                       }

-- | A lens to access the file 'Loader' of a 'Config'.
loader :: Lens' (Config m r) (Loader m)
loader f conf = fmap (`setLoader` conf) (f (confLoader conf))

-- | A lens to access the error handler.
errorHandler :: Lens' (Config m r) (ErrorHandler r)
errorHandler f conf = fmap (`setErrorHandler` conf) (f (confErrorHandler conf))

-- | A lens to access the 'Set' of 'Extension's to enable.
extensions :: Lens' (Config m r) (Set Extension)
extensions f conf = fmap (\xs -> conf { confExtensions = xs }) (f (confExtensions conf))

-- | Set the file 'Loader' in a 'Config'.
setLoader :: Loader m -> Config m r -> Config m r
setLoader l conf = conf { confLoader = l }

-- | Set the file 'Loader' in a 'Config'.
setErrorHandler :: ErrorHandler r -> Config m r -> Config m r
setErrorHandler h conf = conf { confErrorHandler = h }

-- | Add an 'Extension' to enable.
--
-- using lens or lens-family it is
-- @
-- 'addExtension' e = 'set' ('extensions' . contains e) True
-- @
addExtension :: Extension -> Config m r -> Config m r
addExtension x conf =
    conf { confExtensions = Set.insert x (confExtensions conf) }


-- | A van Laarhoven Lens type synonym. Compatible with lens and
-- lens-family.
type Lens' a b = forall f . Functor f => (b -> f b) -> a -> f a

-- | Map a function over the targeted value
over :: Lens' a b -> (b -> b) -> a -> a
over l f = runIdentity . l (Identity . f)

-- | Set the target of a lens to the given value.
set :: Lens' a b -> b -> a -> a
set l = over l . const

-- | Get the target of a lens.
get :: Lens' a b -> a -> b
get l = getConst . l Const

