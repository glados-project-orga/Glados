{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- Type
-}

module Type (
    parseType
    ) where

import Ast
import Parser
import Control.Applicative
import ExpressionInk (parseExpression)

-- Other Types such as "int", "void", "CustomType" [Class/Typedef]
parseOtherType :: Parser Type
parseOtherType =  (keyword "int" *> pure IntType)
    <|> (keyword "void" *> pure VoidType)
    <|> (keyword "long" *> pure LongType)
    <|> (keyword "bool" *> pure BoolType)
    <|> (keyword "string" *> pure StringType)
    <|> (keyword "float" *> pure FloatType)
    <|> (keyword "double" *> pure DoubleType)
    <|> (keyword "char" *> pure CharType)
    <|> (pure CustomType) <*> identifier

-- Array Type shuch as "int[10]" 
parseArrayType :: Parser ArrayVar
parseArrayType = ArrayVar <$> parseOtherType 
    <*> (symbol '[' *> parseExpression <* symbol ']')

-- Checking for array type first 
parseType :: Parser Type
parseType = (ArrayType <$> parseArrayType)
    <|> parseOtherType
