{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module CompileLiteral (compileLiteralExpr) where

import Ast (Literal(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

compileLiteralExpr :: Literal -> CompilerData -> Either String CompilerData
compileLiteralExpr (IntLit n) prog = Right $ appendBody prog ["iconst " ++ show n]
compileLiteralExpr (LongLit n) prog = Right $ appendBody prog ["lconst " ++ show n]
compileLiteralExpr (FloatLit x) prog = Right $ appendBody prog ["fconst " ++ show x]
compileLiteralExpr (DoubleLit x) prog = Right $ appendBody prog ["dconst " ++ show x]
compileLiteralExpr (BoolLit True) prog = Right $ appendBody prog ["iconst 1"]
compileLiteralExpr (BoolLit False) prog = Right $ appendBody prog ["iconst 0"]
compileLiteralExpr (CharLit c) prog = Right $ appendBody prog ["cconst " ++ "'" ++ [c] ++ "'"]
compileLiteralExpr (StringLit s) prog = Right $ appendBody prog ["sconst \"" ++ s ++"\""]

-- compileStringLiteral :: String -> CompilerData -> CompilerData
-- compileStringLiteral s prog = appendBody new_prog ["ldc " ++ show handle]
--   where
--     (new_prog, handle) = storeInConstantPool prog s