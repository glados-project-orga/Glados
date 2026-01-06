{-
-- EPITECH PROJECT, 2025
-- Pandoc Parser Rebased in Right Left
-- File description:
-- Glados Parser
-}

module Parser (
    Parser(..),
    ParseState(..),
    parseChar,
    sepBy,
    parseAnyChar,
    parseSpaces,
    betweenSpaces,
    parseOr,
    parseSome,
    parseMany,
    parseUInt,
    parseInt,
    initialState,
    getSourcePos,
    symbol,
    identifier,
    keyword,
    parseString,
    chainl1
) where

import Ast
import Data.Char (isSpace)
import Control.Applicative

data ParseState = ParseState
  { input    :: String
  , line     :: Int
  , column   :: Int
  }

initialState :: String -> ParseState
initialState content = ParseState
  { input = content
  , line = 1
  , column = 1
  }

data Parser a = Parser {
    runParser :: ParseState -> Either String (a, ParseState)
}

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

instance Functor Parser where
    fmap fct (Parser a) = Parser f
      where
          f str = case a str of
              Right (res, rest) -> Right (fct res, rest)
              Left err -> Left err

instance Applicative Parser where
    pure a = Parser $ \res -> Right (a, res)
    p1 <*> p2 = Parser f
      where
        f str = case runParser p1 str of
            Right (res, rest) -> case runParser p2 rest of
                Left err -> Left err
                Right (res1, rest1) -> Right (res res1, rest1)
            Left err -> Left err

instance Alternative Parser where
    empty = Parser (\_ -> Left "Parser is empty")
    p1 <|> p2 = Parser f
     where
      f str = case runParser p1 str of
         Right (res, rest) -> Right (res, rest)
         Left _ -> runParser p2 str

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \content ->
        case p content of
            Left err -> Left err
            Right (a, rest) -> runParser (f a) rest


getSourcePos :: Parser SourcePos
getSourcePos = Parser $ \st ->
    Right (SourcePos { srcLine = line st, srcColumn = column st}, st)

parseChar :: Char -> Parser Char
parseChar c = Parser $ \st ->
    case input st of 
        [] -> Left $ "Expected '" ++ [c] ++ "' at " ++ show (line st) ++ ":" ++ show (column st)
        (x:xs)
            | x == c && x == '\n' ->
                Right (c, st { input = xs, line = (line st) + 1, column = 1})
            | x == c ->
                Right (c, st { input = xs, line = (line st), column = (column st) + 1})
            | otherwise -> Left $ "Character '" ++ [c] ++ "' not found" ++ "' at " ++ show (line st) ++ ":" ++ show (column st)

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \st ->
    case input st of
        [] -> Left $ "Expected '" ++ str ++ "' at " ++ show (line st) ++ ":" ++ show (column st)
        (x:xs)
            | x `elem` str && x == '\n' ->
                Right (x, st { input = xs, line = (line st) + 1, column = 1})
            | x `elem` str ->
                 Right (x, st { input = xs, line = (line st), column = (column st) + 1})
            | otherwise -> Left $ "Character '" ++ [x] ++ "' not found"
                ++ "' at " ++
                show (line st) ++ ":" ++ show (column st)

handleChar :: ParseState -> Char -> ParseState
handleChar st c | c == '\n' =
    st {input = (drop 1 (input st)), line = (line st) + 1, column = 1}
                | otherwise =
    st {input = (drop 1 (input st)), line = (line st), column = (column st) + 1}


handleSpaces :: ParseState -> ParseState
handleSpaces st = foldl handleChar st (takeWhile isSpace (input st))

parseSpaces :: Parser ()
parseSpaces = Parser $ \st -> Right ((), handleSpaces st)

betweenSpaces :: Parser a -> Parser a
betweenSpaces p = parseSpaces *> p <* parseSpaces

parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseMany :: Parser a -> Parser [a]
parseMany p = (:) <$> p <*> parseMany p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = some p

parseUInt :: Parser Int
parseUInt = read <$> some (parseAnyChar ['0'..'9'])

parseInt :: Parser Int
parseInt = (negate <$> (parseChar '-' *> parseUInt)) <|> parseUInt

token :: Parser a -> Parser a
token p = parseSpaces *> p <* parseSpaces

parseString :: String -> Parser String
parseString [] = pure []
parseString (c:cs) = (:) <$> parseChar c <*> parseString cs

symbol :: Char -> Parser Char
symbol = betweenSpaces . parseChar

keyword :: String -> Parser String
keyword = betweenSpaces . traverse parseChar

identifier :: Parser String
identifier =
    betweenSpaces $
        (:) <$> parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
            <*> parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op =
  foldl (\acc (f, x) -> f acc x) <$> p <*> many ((,) <$> op <*> p)

