{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- ParserSExpr.hs
-}

module ParserSExpr (
    parseSExpr,
) where

import Data(SExpr(..))
import Parser
import Data.Char (isSpace)
import Control.Applicative

parseSymboleChar :: Parser Char
parseSymboleChar = Parser f
  where
    f [] = Left "Reached end of input"
    f (x:xs)
      | not (isSpace x) && x /= '(' && x /= ')' = Right (x, xs)
      | otherwise = Left $ "Invalid Symbole char: " ++ show x

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> some parseSymboleChar

parseSNumber :: Parser SExpr
parseSNumber = SInt <$> parseInt

parseSList :: Parser SExpr
parseSList = SList <$> (parseChar '(' *> many parseSExpr <* parseChar ')')

parseSExpr :: Parser SExpr
parseSExpr = betweenSpaces (
    parseSNumber <|>
    parseSSymbol <|>
    parseSList
    )

-- main :: IO ()
-- main = print (runParser parseSExpr "(hello (42 world) 123)")