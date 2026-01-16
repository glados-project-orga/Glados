{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- compile block
-}

module CompileBlock (compileBlock) where

import Ast (Statement)
import CompilerTypes (CompilerData)

compileBlock
  :: (Statement -> CompilerData -> Either String CompilerData)
  -> [Statement]
  -> CompilerData
  -> Either String CompilerData
compileBlock _ [] prog = Right prog
compileBlock compileStmt (s:ss) prog =
  case compileStmt s prog of
    Left err -> Left err
    Right prog' -> compileBlock compileStmt ss prog'
