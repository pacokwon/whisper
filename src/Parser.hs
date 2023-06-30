{-# LANGUAGE OverloadedStrings #-}

module Parser (Sexpr (..), number, ident, paren, sexpr, parse) where

import Data.Functor (($>))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, runParser, satisfy, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Sexpr
  = Paren [Sexpr]
  | Number Int
  | Boolean Bool
  | Ident String
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

number :: Parser Sexpr
number = Number <$> lexeme L.decimal

boolean :: Parser Sexpr
boolean = Boolean <$> lexeme (parseTrue <|> parseFalse)
  where
    parseTrue = string "true" $> True
    parseFalse = string "false" $> False

symbols :: Set.Set Char
symbols = Set.fromList "!\"#$%&'*+,-./:<=>?@^_~|"

symbolChar :: Parser Char
symbolChar = satisfy (`Set.member` symbols)

ident :: Parser Sexpr
ident = Ident <$> lexeme p
  where
    first = letterChar <|> symbolChar
    rest = many (alphaNumChar <|> symbolChar)
    p = (:) <$> first <*> rest

paren :: Parser Sexpr
paren = Paren <$> lexeme (char '(' *> some sexpr <* char ')')

sexpr :: Parser Sexpr
sexpr = paren <|> number <|> boolean <|> ident

parse :: Text -> Sexpr
parse input = case runParser sexpr "" input of
  Right s -> s
  Left e -> error . show $ e
