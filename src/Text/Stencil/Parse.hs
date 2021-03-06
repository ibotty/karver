-- |
-- Module:      Data.Stencil.Parse
-- Copyright:   Tobias Florek 2014
-- License:     BSD3
--
-- Maintainer:  Tobias Florek <tob@butter.sh>
-- Stability:   experimental
-- Portability: unknown
--
-- All the 'Parser's are defined here, including the one used by the top
-- level module "Text.Stencil".

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Stencil.Parse
( templateParserExt
, templateParser
, commentParser
, commentParser'
, rawParser
, rawParser'
, literalParser
, variableParser
, variableParser'
, conditionParser
, loopParser
, includeParser
) where

import Text.Stencil.Types

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative  (pure, (*>), (<$), (<$>), (<*), (<*>))
import Data.Monoid          (mappend, mempty)
#endif

import Control.Applicative  ((<|>))
import Control.Monad        (void)
import Data.Attoparsec.Text
import Data.Function        (fix)
import Data.Text            (Text)

import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB

templateParser :: (JinjaSYM repr, JinjaVariableSYM repr, JinjaIncludeSYM repr)
               => Parser repr
templateParser = fix includeParserExt

templateParserExt :: (JinjaSYM repr, JinjaVariableSYM repr) => Parser repr -> Parser repr
templateParserExt self = choice
  [ variableParser
  , conditionParser self
  , loopParser self
  , rawParser
  , commentParser
  , literalParser
  ]

includeParserExt :: (JinjaSYM repr, JinjaVariableSYM repr, JinjaIncludeSYM repr)
                 => Parser repr -> Parser repr
includeParserExt self = templateParserExt self <|> includeParser

commentParser :: JinjaSYM r => Parser r
commentParser = pure (literal mempty) <* commentParser'

commentParser' :: Parser ()
commentParser' = "{#" *> inComment

inComment :: Parser ()
inComment =
        "#}" *> endOfLine
    <|> () <$ "#}"
    <|> commentParser' *> inComment
    <|> skipMany1 (takeWhile1 (notInClass startEndChars)) *> inComment
    <|> satisfy (inClass startEndChars) *> inComment
  where
    startEndChars = "{}#"

rawParser :: JinjaSYM r => Parser r
rawParser = literal <$> rawParser'

rawParser' :: Parser TLB.Builder
rawParser' = expressionDelimiter "raw" *> inRawParser

inRawParser :: Parser TLB.Builder
inRawParser =  (expressionDelimiter "endraw" *> pure mempty)
           <|> (mappend <$> fmap TLB.fromText (takeWhile1 (notInClass startEndChars)) <*> inRawParser)
           <|> (mappend <$> fmap TLB.singleton (satisfy (inClass startEndChars)) <*> inRawParser)
    -- mconcat . map TLB.fromText <$> chunksTill '{' (expressionDelimiter "endraw"))
  where
    startEndChars = "{}% endraw\t"

-- | Takes everything until it reaches a @{@, resulting in the 'LiteralTok'
literalParser :: JinjaSYM repr => Parser repr
literalParser = literal . TLB.fromText <$> takeWhile1 (/= '{')

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
variableParser :: JinjaVariableSYM repl => Parser repl
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
conditionParser :: JinjaVariableSYM repr => Parser repr -> Parser repr
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
loopParser :: JinjaVariableSYM repr => Parser repr -> Parser repr
loopParser self = do
  (arr, var) <- expressionDelimiter $ do
    void "for"
    skipSpace
    varName <- Identifier <$> takeTill (== ' ')
    skipSpace
    void "in"
    skipSpace
    arrName <- variableParser'
    return (arrName, varName)
  loopbody <- skipSpaceTillEOL *> many' self <* expressionDelimiter "endfor"
  skipSpaceTillEOL
  return $ loop arr var loopbody

-- | 'Parser' for includes, that will result in 'IncludeTok'
includeParser :: JinjaIncludeSYM repr => Parser repr
includeParser = expressionDelimiter $ do
  void "include"
  skipSpace
  include . T.unpack <$> (quoted '"' <|> quoted '\'')
  where
    quoted c = char c *> takeTill (== c) <* char c
