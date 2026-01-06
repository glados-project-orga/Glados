{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- InkParser
-}

module InkParser (
    parseInkFile
    ) where

import Ast
import Debug.Trace
import Control.Applicative
import Parser
import ExpressionInk

parseType :: Parser Type
parseType =
        (keyword "int"  *> pure IntType)
    <|> (keyword "void" *> pure VoidType)
    <|> (keyword "bool" *> pure BoolType)
    <|> (keyword "string" *> pure StringType)
    <|> (keyword "float" *> pure FloatType)
    <|> (keyword "char" *> pure CharType)

parseDeclaration :: Parser Declaration
parseDeclaration =
        parseFunction
--     <|> parseStruct
--     <|> parseEnum
--     <|> parseTypedef

parseVarDecl :: Parser Statement
parseVarDecl = VarDeclStmt <$> varDecl
  where
    varDecl = VarDecl
        <$> ((keyword "let" *> identifier)
             <|> (keyword "const" *> identifier))
        <*> (keyword "->" *> parseType)
        <*> (symbol '=' *> parseExpression <* symbol ';')
        <*> pure False
        <*> pure False

parseForAssignment :: Parser Statement
parseForAssignment = AssignmentStmt <$> assignment
    where 
    assignment = Assignment
        <$> identifier
        <*> (symbol '=' *> parseExpression)

parseAssignment :: Parser Statement
parseAssignment = AssignmentStmt <$> assignment
    where 
    assignment = Assignment
        <$> identifier
        <*> (symbol '=' *> parseExpression <* symbol ';')

parseIfStmt :: Parser Statement
parseIfStmt = IfStatement <$> ifStmt
    where
    ifStmt = IfStmt
        <$> (keyword "il" *> parseExpression)
        <*> parseBlock
        <*> optional (keyword "elle" *> parseBlock)

parseWhileStmt :: Parser Statement
parseWhileStmt = WhileStatement <$> whileStmt
    where
    whileStmt = WhileStmt
        <$> (keyword "while" *> parseExpression)
        <*> parseBlock

parseForUpdate :: Parser ForUpdate
parseForUpdate =
        ForUpdateStmt <$> parseForAssignment
    <|> ForUpdateExpr <$> parseExpression

initFor :: Parser (Maybe Statement)
initFor =
        (Just <$> parseVarDecl)
    <|> (symbol ';' *> pure Nothing)

parseForStmt :: Parser Statement
parseForStmt = ForStatement <$> forStmt
  where
    forStmt = ForStmt
            <$> (keyword "for" *> symbol '(' *> initFor)
            <*> parseExpression
            <*> (symbol ';' *> parseForUpdate)
            <*> (symbol ')' *> parseBlock)
parseForEachStmt :: Parser Statement
parseForEachStmt = ForEachStatement <$> forEachStmt
  where
    forEachStmt = ForEachStmt
        <$> (keyword "for" *> identifier <* keyword "in")
        <*> parseExpression
        <*> parseBlock

parseMatchStmt :: Parser Statement
parseMatchStmt = MatchStatement <$> matchStmt
  where
    matchStmt = MatchStmt
        <$> (keyword "match" *> parseExpression)
        <*> parseMany parseMatchCase

parseMatchCase :: Parser MatchCase
parseMatchCase = matchCase
  where
    matchCase = MatchCase
        <$> (parsePattern <* keyword "=>")
        <*> parseExpression

parsePattern :: Parser Pattern
parsePattern = literalPattern <|> defaultPattern
  where
    literalPattern = LiteralPattern <$> parseLiteral
    defaultPattern = symbol '_' *> pure DefaultPattern

parseTryCatchStmt :: Parser Statement
parseTryCatchStmt = TryCatchStatement <$> tryCatchStmt
  where
    tryCatchStmt = TryCatchStmt
        <$> (keyword "try" *> parseBlock)
        <*> (keyword "catch" *> identifier)
        <*> identifier
        <*> parseBlock

parseThrowStmt :: Parser Statement
parseThrowStmt = ThrowStatement <$> throwStmt
  where
    throwStmt = ThrowStmt
        <$> (keyword "throw" *> identifier)
        <*> parseExpression

parseReturnStmt :: Parser Statement
parseReturnStmt = ReturnStatement <$> returnStmt
  where
    returnStmt = ReturnStmt
        <$> (keyword "=>" *> parseExpression <* symbol ';')

parseExprStmt :: Parser Statement
parseExprStmt = ExprStatement <$> exprStmt
  where
    exprStmt = ExprStmt <$> ((parseParens parseExpression)
                        <|> (parseExpression <* symbol ';')
                        <|> (parseExpression <* symbol ')'))

parseParens :: Parser Expr -> Parser Expr
parseParens p = 
    (symbol '(' *> p <* symbol ')')

parseStatement :: Parser Statement
parseStatement = parseVarDecl
                <|> parseAssignment
                <|> parseIfStmt
                <|> parseWhileStmt
                <|> parseForStmt
                <|> parseForEachStmt
                <|> parseMatchStmt
                <|> parseTryCatchStmt
                <|> parseThrowStmt
                <|> parseReturnStmt
                <|> parseExprStmt
                <|> invalidToken

invalidToken :: Parser a
invalidToken = Parser $ \st ->
    case input st of
        [] -> Left "Unexpected end of input"
        (c:_) -> Left $ "Invalid token '" ++ [c] ++ "' at line "
                        ++ show (line st) ++ ", column " ++ show (column st)


parseBlock :: Parser [Statement]
parseBlock = symbol '{' *> parseMany parseStatement <* symbol '}'

parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl =
    FunctionDecl
        <$> getSourcePos
        <*> (keyword "fun" *> identifier)
        <*> (symbol '(' *> symbol ')' *> pure [])
        <*> (symbol ':' *> parseType)
        <*> parseBlock

parseFunction :: Parser Declaration
parseFunction =
    Function <$> parseFunctionDecl


parseInkFile :: String -> IO (Either String [Declaration])
parseInkFile content =
    case runParser (parseMany parseDeclaration) (initialState content) of
        Right (decl, _) -> return $ Right decl
        Left err -> return $ Left err