-- |
-- Module:      Data.Karver.Parse
-- Copyright:   Jeremy Hull 2013
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- All the 'Parser's are defined here, including the one used by the top
-- level module "Text.Karver".

{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( templateParserExt
, templateParser
, literalParser
, variableParser
, variableParser'
, conditionParser
, loopParser
, includeParser
) where

import Text.Karver.Types

import Control.Applicative  ((*>), (<$>), (<*), (<|>))
import Data.Attoparsec.Text
import Data.Function        (fix)
import Data.Text            (Text)

import qualified Data.Text as T

templateParser :: JinjaIncludeSYM repr => Parser repr
templateParser = fix includeParserExt

-- | Top level 'Parser' that will translate 'Text' into ['Token']
templateParserExt :: JinjaSYM repr => Parser repr -> Parser repr
templateParserExt self = choice
  [ variableParser
  , conditionParser self
  , loopParser self
  , literalParser
  ]

includeParserExt :: JinjaIncludeSYM repr => Parser repr -> Parser repr
includeParserExt self = templateParserExt self <|> includeParser


-- | Takes everything until it reaches a @{@, resulting in the 'LiteralTok'
literalParser :: JinjaSYM repr => Parser repr
literalParser = literal <$> takeWhile1 (/= '{')

-- General function for making parsers that will be surrounded by a curtain
-- delimiter — which has both a beginning and end.
delimiterParser :: Text -> Text -> Parser a -> Parser a
delimiterParser begin end parser =
  string begin *> skipSpace *> parser <* skipSpace <* string end

identityDelimiter, expressionDelimiter :: Parser a -> Parser a

identityDelimiter   = delimiterParser "{{" "}}"
expressionDelimiter = delimiterParser "{%" "%}"


-- General parser for the several variable types. It is basically used to
-- not repeat parsers with and without a delimiter.
variableParser_ :: (Parser Variable -> Parser Variable) -> Parser Variable
variableParser_ fn = fn $ do
  ident <- takeTill (inClass " .[}%")
  peek <- peekChar
  case peek of
    (Just '[') ->
           ListIndex ident <$> inBrackets decimal
       <|> ObjectKey ident <$> inBrackets (quoted identifierName)
    (Just '.') -> ObjectKey ident <$> (char '.' *> identifierName)
    (Just ' ') -> return $ Variable ident
    (Just '}') -> return $ Variable ident
    (Just '%') -> return $ Variable ident
    Nothing    -> return $ Variable ident
    _          -> fail "variableParser_: failed with no token to apply."
  where
    inBrackets = delimiterParser "[" "]"
    quoted p = char '"' *> p <* char '"' <|> char '\'' *> p <* char '\''
    identifierName = takeTill (inClass " '\"%}")

-- | 'Parser' for all the variable types. Returning on of the following
-- 'Token's:
--
-- * 'IncludeTok'
--
-- * 'ListTok'
--
-- * 'ObjectTok'
variableParser :: JinjaSYM repl => Parser repl
variableParser  = variable <$> variableParser_ identityDelimiter

-- | 'Parser' for all the variable types. Returning on of the following
-- 'Token's:
--
-- * 'IncludeTok'
--
-- * 'ListTok'
--
-- * 'ObjectTok'
--
-- This is without the delimiter
variableParser' :: Parser Variable
variableParser' = variableParser_ id

-- Parser for skipping over horizontal space and end on a newline
-- character, which will be skipped as well.
skipSpaceTillEOL :: Parser ()
skipSpaceTillEOL = option () $ skipWhile isHorizontalSpace >> endOfLine
{-# INLINE skipSpaceTillEOL #-}

-- | 'Parser' for if statements, that will result in the 'ConditionTok'
conditionParser :: JinjaSYM repr => Parser repr -> Parser repr
conditionParser self = do
  logic <- expressionDelimiter $ "if" *> skipSpace *> variableParser'
  ifbody <- ifparse
  elsebody <- elseparse
  skipSpaceTillEOL
  return $ condition (VariableNotNull logic) ifbody elsebody

  where
    ifparse = skipSpaceTillEOL *> many' self <* expressionDelimiter ("endif" <|> "else")
    elseparse = option [] $ skipSpaceTillEOL *> many' self <* expressionDelimiter "endif"

-- | 'Parser' for for loops, that will result in the 'LoopTok'
loopParser :: JinjaSYM repr => Parser repr -> Parser repr
loopParser self = do
  (arr, var) <- expressionDelimiter $ do
    "for"
    skipSpace
    varName <- Identifier <$> takeTill (== ' ')
    skipSpace
    "in"
    skipSpace
    arrName <- variableParser'
    return (arrName, varName)
  loopbody <- skipSpaceTillEOL *> many' self <* expressionDelimiter "endfor"
  skipSpaceTillEOL
  return $ loop arr var loopbody

-- | 'Parser' for includes, that will result in 'IncludeTok'
includeParser :: JinjaIncludeSYM repr => Parser repr
includeParser = expressionDelimiter $ do
  let quoted c = char c *> takeTill (== c) <* char c
  "include"
  skipSpace
  include . T.unpack <$> (quoted '"' <|> quoted '\'')
