{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- if
-}

module If (compileIf) where

import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Ast (IfStmt(..), Statement)
import Expr (compileExpr)
import Labels (generateLabel)
import CompileBlock (compileBlock)

compileIf
  :: (Statement -> CompilerData -> Either String CompilerData)
  -> IfStmt
  -> CompilerData
  -> Either String CompilerData
compileIf compileStmt ifs prog =
  case compileExpr (ifCondition ifs) prog of
    Left err -> Left err
    Right p1 ->
      case ifElseBody ifs of
        Nothing      -> compileIfNoElse compileStmt ifs p1
        Just elseBlk -> compileIfWithElse compileStmt ifs elseBlk p1

compileIfNoElse
  :: (Statement -> CompilerData -> Either String CompilerData)
  -> IfStmt
  -> CompilerData
  -> Either String CompilerData
compileIfNoElse compileStmt ifs prog =
  let (lEnd, p1) = generateLabel prog
      p2 = emit ("ifeq " ++ lEnd) p1
  in case compileBlock compileStmt (ifThenBody ifs) p2 of
       Left err -> Left err
       Right p3 -> Right (emit (lEnd ++ ":") p3)

compileIfWithElse
  :: (Statement -> CompilerData -> Either String CompilerData)
  -> IfStmt
  -> [Statement]
  -> CompilerData
  -> Either String CompilerData
compileIfWithElse compileStmt ifs elseBlk prog =
  let (lElse, p1) = generateLabel prog
      (lEnd,  p2) = generateLabel p1
      p3 = emit ("ifeq " ++ lElse) p2
  in case compileBlock compileStmt (ifThenBody ifs) p3 of
       Left err -> Left err
       Right p4 ->
         let p5 = emit ("goto " ++ lEnd) p4
             p6 = emit (lElse ++ ":") p5
         in case compileBlock compileStmt elseBlk p6 of
              Left err -> Left err
              Right p7 -> Right (emit (lEnd ++ ":") p7)

emit :: String -> CompilerData -> CompilerData
emit instr prog = appendBody prog [instr]
