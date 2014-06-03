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
import Data.Aeson (decode')
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V

-- | Renders a template
renderTemplate :: (Functor m, Monad m)
               => (Text -> m Text)   -- ^ load templates that are included
               -> HashMap Text Value -- ^ Data map for variables inside
                                     --   a given template
               -> Text               -- ^ Template
               -> m Text
renderTemplate include varTable = encode
  where -- commented type sigs to resolv ambigious types
        -- encode :: (Functor m, Monad m) => Text -> m Text
        encode tlp
          | T.null tlp = return tlp
          | otherwise  = merge $
              case parseOnly templateParser tlp of
                (Left err)  -> [LiteralTok $ T.pack err]
                (Right res) -> res

        -- merge :: (Functor m, Monad m) => [Token] -> m Text
        merge = fmap T.concat . mapM (decodeToken varTable)

        -- decodeToken :: (Functor m, Monad m) => HashMap Text Value -> Token -> m Text
        decodeToken _ (LiteralTok x)       = return x
        decodeToken vTable (IdentityTok x) = return $
          case H.lookup x vTable of
            (Just (Literal s)) -> s
            _                 -> T.empty
        decodeToken vTable (ObjectTok i k) = return $
          case H.lookup i vTable of
            (Just (Object m)) ->
              case H.lookup k m of
                (Just x) -> x
                Nothing  -> T.empty
            _              -> T.empty
        decodeToken vTable (ListTok a i) = return $
          case H.lookup a vTable of
            (Just (List l)) -> case l V.! i of
                                 (Literal t) -> t
                                 _           -> T.empty
            _               -> T.empty
        decodeToken _ (ConditionTok c t f) = do
          b <- hasVariable c
          encode $ if b
                       then t
                       else f
          where hasVariable txt =
                  case parseOnly variableParser' txt of
                    (Right res) -> not . T.null <$> decodeToken varTable res
                    _           -> return False
        decodeToken vTable (LoopTok a v b) =
          case H.lookup a vTable of
            (Just (List l)) -> do
              let toks = case parseOnly templateParser b of
                           (Left _)  -> []
                           (Right res) -> res
                  mapVars x = let vTable' = H.insert v x vTable
                              in mapM (decodeToken vTable') toks
              if null toks
                   then return T.empty
                   else T.concat . V.toList <$> V.mapM (fmap T.concat . mapVars) l
            _               -> return T.empty
        decodeToken _ (IncludeTok f) =
          encode . T.init =<< include f

loadTemplateFromDisc :: Text -> IO Text
loadTemplateFromDisc = TI.readFile . T.unpack

-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate' :: Text -- ^ JSON data, for variables inside a given
                        --   template
                -> Text -- ^ Template
                -> IO Text
renderTemplate' json tpl =
  case decode' . L.pack $ T.unpack json of
    (Just hash) -> renderTemplate loadTemplateFromDisc hash tpl
    Nothing     -> return T.empty
