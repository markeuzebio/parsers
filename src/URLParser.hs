{-# LANGUAGE OverloadedStrings #-}  -- So strings literals are not necessarily interpreted as String type
{-# LANGUAGE RecordWildCards   #-}


-- The main goal here is to parse URIs of the following form:
--  scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

module URLParser where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum)

type Parser = Parsec Void Text

type User = Text
type Password = Text

data Uri = Uri 
    { 
        uriScheme :: Scheme,
        uriAuthority :: Maybe Authority,
        uriPath :: Text,
        uriQuery :: Maybe Text,
        uriFragment :: Maybe Text
    } 
    deriving (Eq, Show)

data Authority = Authority
    {
        authUser :: Maybe (User, Password),
        authHost :: Text,
        authPort :: Maybe Int
    }
    deriving (Eq, Show)

data Scheme 
    = SchemeData
    | SchemeFile
    | SchemeFtp
    | SchemeHttp
    | SchemeHttps
    | SchemeIrc
    | SchemeMailTo
    deriving (Eq, Show)

isPathChar :: Char -> Bool
isPathChar c = isAlphaNum c || c `elem` ("/.-_~%" :: String)

pUri :: Parser Uri
pUri = do
    uriScheme <- pScheme <?> "valid scheme" 
    void (char ':')
    uriAuthority <- optional $ do
        void (string "//")
        authUser <- optional . try $ do
            user <- T.pack <$> some alphaNumChar <?> "username"
            void (char ':')
            password <- T.pack <$> some alphaNumChar <?> "password"
            void (char '@')
            return (user, password)
        authHost <- T.pack <$> some (alphaNumChar <|> oneOf ("-." :: String)) <?> "hostname"
        authPort <- optional (char ':' *> label "port number" L.decimal)
        return Authority { .. }
    uriPath <- T.pack <$> some (satisfy isPathChar) <?> "path"
    uriQuery <- optional $ do
        void (char '?')
        T.pack <$> many (anySingleBut '#') <?> "query"
    uriFragment <- optional $ do
        void (char '#')
        T.pack <$> many anySingle <?> "fragment"
    eof
    return Uri { .. }

pScheme :: Parser Scheme
pScheme = choice [SchemeData <$ string "data"
    , SchemeFile <$ string "file"
    , SchemeFtp <$ string "ftp"
    , SchemeHttps <$ string "https"
    , SchemeHttp <$ string "http"
    , SchemeIrc <$ string "irc"
    , SchemeMailTo <$ string "mailto"]


alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar
    where 
        foo = (,) <$> char 'a' <*> char 'b'
        bar = (,) <$> char 'a' <*> char 'c'