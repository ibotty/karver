module Text.Stencil.Config
  ( module Text.Stencil.Config
  ) where

import Text.Stencil.Types

import Data.Set (Set)

import qualified Data.Set as Set

data Extension = LoadIncludes
  deriving (Eq, Show, Read, Ord, Bounded)

data Config m = Config
  { confLoader :: Loader m
  , confExtensions :: Set Extension
  , confErrorHandler :: ErrorHandler
  }

defaultConfig :: Monad m => Config m
defaultConfig = Config { confLoader = return . const Nothing
                       , confExtensions = Set.singleton LoadIncludes
                       , confErrorHandler = Left
                       }

-- | A lens to access the file 'Loader' of a 'Config'.
--
-- @
-- 'loader' :: Lens' ('Config' m) ('Loader' m)
-- @
loader :: Functor f => (Loader m -> f (Loader m)) -> Config m -> f (Config m)
loader f conf = fmap (`setLoader` conf) (f (confLoader conf))

-- | A lens to access the error handler.
--
-- @
-- 'loader' :: Lens' ('Config' m) ('ErrorHandler' m)
-- @
errorHandler :: Functor f => (ErrorHandler -> f ErrorHandler) -> Config m -> f (Config m)
errorHandler f conf = fmap (`setErrorHandler` conf) (f (confErrorHandler conf))

-- | A lens to access the 'Set' of 'Extension's to enable.
--
-- @
-- 'extensions' :: Lens' ('Config' m) ('Set' 'Extension')
-- @
extensions :: Functor f => (Set Extension -> f (Set Extension)) -> Config m -> f (Config m)
extensions f conf = fmap (\xs -> conf { confExtensions = xs }) (f (confExtensions conf))

-- | Set the file 'Loader' in a 'Config'.
--
-- using lens or lens-family it is
-- @
-- 'setLoader' = set 'loader'
-- @
setLoader :: Loader m -> Config m -> Config m
setLoader l conf = conf { confLoader = l }

-- | Set the file 'Loader' in a 'Config'.
--
-- using lens or lens-family it is
-- @
-- 'setLoader' = set 'loader'
-- @
setErrorHandler :: ErrorHandler -> Config m -> Config m
setErrorHandler h conf = conf { confErrorHandler = h }

-- | Add an 'Extension' to enable.
--
-- using lens or lens-family it is
-- @
-- 'addExtension' e = set ('extensions' . contains e) True
-- @
addExtension :: Extension -> Config m -> Config m
addExtension x conf =
    conf { confExtensions = Set.insert x (confExtensions conf) }
