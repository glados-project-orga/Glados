{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- StatementParser
-}

module StatementParser (
    parseStatement,
    parseBlock,
    parseLiteral,
    parseType
) where

import Ast
import Control.Applicative
import Parser
import Literal
import Type
import ExpressionInk

parseLambdaVar :: Parser LambdaVar
parseLambdaVar =
    LambdaVar
        <$> (symbol '(' *> sepBy parseType comma <* symbol ')')
        <*> (keyword "=>" *> parseType <* symbol '=')
        <*> (parseLambdaParamNames)
        <*> parseBlock <* symbol ';'

parseLambdaParamNames :: Parser [String]
parseLambdaParamNames =
        symbol '[' *> sepBy identifier comma <* symbol ']'
    <|> pure []

parseIsConst :: Parser Bool
parseIsConst = (keyword "const" *> pure True)
                <|> (keyword "let" *> pure False)

parseIsRef :: Parser Bool
parseIsRef = (symbol '&' *> pure True)
            <|> pure False

parseLambdaDecl :: Parser Statement
parseLambdaDecl = LambdaStatement
        <$> (LambdaDecl
                <$> (keyword "let" *> identifier)
                <*> (keyword "->" *> parseLambdaVar)
            )

parseVarDecl :: Parser Statement
parseVarDecl =
    VarDeclStmt
        <$> ((\isConst isRef name typ val ->
                VarDecl
                    { varName    = name
                    , varType    = typ
                    , varValue   = val
                    , varIsConst = isConst
                    , varIsRef   = isRef
                    }
            )
            <$> parseIsConst
            <*> parseIsRef
            <*> identifier
            <*> (keyword "->" *> parseType)
            <*> (symbol '=' *> parseExpression <* symbol ';'))

parseForAssignment :: Parser Statement
parseForAssignment = AssignmentStmt <$> assignment
    where 
    assignment = Assignment
        <$> parseExpression
        <*> (symbol '=' *> parseExpression)

parseAssignment :: Parser Statement
parseAssignment = AssignmentStmt <$> assignment
    where 
    assignment = Assignment
        <$> parseExpression
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
        <*> (symbol '{' *> parseMany parseMatchCase <* symbol '}')

parseMatchCase :: Parser MatchCase
parseMatchCase = matchCase
  where
    matchCase = MatchCase
        <$> (parsePattern <* keyword "=>")
        <*> (parseExpression) <* symbol ';'

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
        <*> (keyword "catch" *> symbol '(' *> optional identifier <* symbol ')')
        <*> parseBlock

parseThrowStmt :: Parser Statement
parseThrowStmt = ThrowStatement <$> throwStmt
  where
    throwStmt = ThrowStmt
        <$> (keyword "throw" *> parseExpression <* symbol ';')

parseReturnStmt :: Parser Statement
parseReturnStmt = ReturnStatement <$> returnStmt
  where
    returnStmt = ReturnStmt
        <$> (keyword "=>" *> parseExpression <* symbol ';')

parseExprStmt :: Parser Statement
parseExprStmt = ExprStatement <$> exprStmt
  where
    exprStmt = ExprStmt <$> parseExpression <* symbol ';'

parseStatement :: Parser Statement
parseStatement = parseVarDecl
                <|> parseLambdaDecl
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
