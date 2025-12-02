{-
-- EPITECH PROJECT, 2025
-- Pandoc Parser Rebased in Right Left
-- File description:
-- Glados Parser
-}

module Parser (
    Parser(..),
    parseChar,
    sepBy,
    parseAnyChar,
    parseSpaces,
    betweenSpaces,
    parseOr,
    parseSome,
    parseMany,
    parseUInt,
    parseInt
) where

import Data.Char (isSpace)
import Control.Applicative

data Parser a = Parser {
    runParser :: String -> Either String (a, String)
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
    p1 <|>  p2 = Parser f
     where
      f str = case runParser p1 str of
         Right (res, rest) -> Right (res, rest)
         Left err -> runParser p2 str

parseChar :: Char -> Parser Char
parseChar c = Parser f
    where f [] = Left  $ "Expected '" ++ [c] ++ "' but reached end of input"
          f (x:xs)
            | x == c = Right (c, xs)
            | otherwise = Left $ "Character '" ++ [c] ++ "' not found"

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser f
    where f [] = Left $ "Reached end of input"
          f (x:xs)
            | x `elem` str = Right (x, xs)
            | otherwise = Left $ "Character '" ++
            [x] ++
            "' not found in the entier string"

parseSpaces :: Parser ()
parseSpaces = Parser $ \input -> Right ((), dropWhile isSpace input)

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

