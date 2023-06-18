module Parser (Sexpr (..), number, ident, paren, sexpr) where

import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, satisfy, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Sexpr
  = Paren [Sexpr]
  | Number Int
  | Ident String
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

number :: Parser Sexpr
number = Number <$> lexeme L.decimal

symbols :: Set.Set Char
symbols = Set.fromList "!\"#$%&'*+,-./:<=>?@^_~"

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
sexpr = paren <|> number <|> ident
