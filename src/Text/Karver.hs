{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      Data.Karver
-- Copyright:   Jeremy Hull 2013
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- The "Text.Karver" interface for translation 'Text' from it's template
-- syntax, to a generated value — based on the data that was given.

module Text.Karver
( renderTemplate
, renderTemplate'
, module Text.Karver.Types
) where

import Text.Karver.Types
import Text.Karver.Parse

import Control.Applicative ((<$>))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import Data.Aeson (decode')
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Monoid (Monoid(mappend, mempty))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Typeable (Typeable)
import qualified Data.Vector as V

data KarverError = InvalidTemplate Text String
                 | NoSuchInclude Text
                 | LookupError Text
                 | ManyErrors [KarverError]
  deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid (KarverError) where
  mappend e e' = ManyErrors [e, e']
  mempty = ManyErrors []

-- | Renders a template
renderTemplate :: (Functor m, Monad m)
               => (Text -> m (Maybe Text))   -- ^ load templates that are included
               -> (KarverError -> Either KarverError Text)
               -> HashMap Text Value -- ^ Data map for variables inside
                                     --   a given template
               -> Text               -- ^ Template
               -> m (Either KarverError Text)
renderTemplate includer errHandler varTable = runExceptT . encode
  where -- commented type sigs to resolv ambigious types
        errHandler' = ExceptT . return . errHandler

        -- encode :: (Functor m, Monad m) => Text -> ExceptT KarverError m Text
        encode tlp
          | T.null tlp = return T.empty
          | otherwise  =
                  case parseOnly templateParser tlp of
                    Right tokens -> merge tokens
                    Left err     -> errHandler' $ InvalidTemplate tlp err

        -- merge :: (Functor m, Monad m) => [Token] -> ExceptT KarverError m Text
        merge tokens = T.concat <$> mapM (decodeToken varTable) tokens

        -- decodeToken :: (Functor m, Monad m) => HashMap Text Value -> Token -> ExceptT KarverError m Text
        decodeToken _ (LiteralTok x)       = return x
        decodeToken vTable (IdentityTok x) =
          case H.lookup x vTable of
            (Just (Literal s)) -> return s
            _                  -> errHandler' $ LookupError x
        decodeToken vTable (ObjectTok i k) =
          case H.lookup i vTable of
            (Just (Object m)) -> case H.lookup k m of
                                   (Just x) -> return x
                                   Nothing  -> errHandler' $ LookupError i
            _                 -> errHandler' $ LookupError k
        decodeToken vTable (ListTok a i) =
          case H.lookup a vTable of
            (Just (List l)) -> case l V.! i of
                                 (Literal t) -> return t
                                 _           -> errHandler' $ LookupError a
            _               -> errHandler' $ LookupError a
        decodeToken _ (ConditionTok c t f) =
          hasVariable c >>= \b -> encode $ if b then t else f
          where hasVariable txt =
                  case parseOnly variableParser' txt of
                    (Right res) -> hasVariable' <$> decodeToken varTable res
                    (Left err)  -> case errHandler (InvalidTemplate txt err) of
                                     Left err' -> throwE err'
                                     Right res -> return . hasVariable' $ res
                hasVariable' = not . T.null
        decodeToken vTable (LoopTok a v b) =
          case H.lookup a vTable of
            (Just (List l)) -> do
              let toks = case parseOnly templateParser b of
                           (Left _)    -> [] -- XXX call error handler
                           (Right res) -> res :: [Token]
                  mapVars x = let vTable' = H.insert v x vTable
                              in mapM (decodeToken vTable') toks
              if null toks
                   then return T.empty
                   else T.concat . V.toList <$> V.mapM (fmap T.concat . mapVars) l
            _               -> errHandler' $ LookupError a
        decodeToken _ (IncludeTok f) =
          ExceptT (Right <$> includer f) >>= \case
                            Nothing -> errHandler' $ NoSuchInclude f
                            Just tmpl -> encode $ T.init tmpl

loadTemplateFromDisc :: Text -> IO (Maybe Text)
loadTemplateFromDisc = fmap return . TI.readFile . T.unpack

continueHandler :: KarverError -> Either KarverError Text
continueHandler (InvalidTemplate tmpl _) = Right tmpl
continueHandler (LookupError _) = Right T.empty
continueHandler (NoSuchInclude err) = Right err
continueHandler (ManyErrors _) = Right T.empty

-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate' :: Text -- ^ JSON data, for variables inside a given
                        --   template
                -> Text -- ^ Template
                -> IO Text
renderTemplate' json tpl =
  case decode' . L.pack $ T.unpack json of
    (Just hash) -> renderTemplate loadTemplateFromDisc continueHandler hash tpl >>= \case
                     Left err -> error $ "renderTemplate': something went wrong: " ++ show err
                     Right a -> return a
    Nothing     -> return T.empty
