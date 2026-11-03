{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

data Expr
    = Var String
    | Int Int
    | Negation Expr
    | Sum Expr Expr
    | Subtr Expr Expr
    | Product Expr Expr
    | Division Expr Expr
    deriving (Show, Eq)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
        space1
        (L.skipLineComment "#")
        (L.skipBlockComment "'''" "'''")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVariable :: Parser Expr
pVariable = Var <$> lexeme ((:) <$> firstChar <*> many leftChars <?> "variable")
    where
        firstChar = letterChar <|> char '_'
        leftChars = alphaNumChar <|> char '_'

pInteger :: Parser Expr
pInteger = Int <$> lexeme (L.signed sc L.decimal)

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
    [
        pParens pExpr,
        pVariable,
        pInteger
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [
            prefix "-" Negation,
            prefix "+" id
        ],
        [
            binary "*" Product,
            binary "/" Division
        ],
        [
            binary "+" Sum,
            binary "-" Subtr
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)