{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr) where

import Ast (Expr(..), CallExpr(..), FunctionDecl(..))
import CompilerTypes (CompilerData)
import Lookup (getFunctions, findFunction)
import Arity (checkArity)
import ExprArgs (compileArgs)
import CompilerTools (appendBody)

emitCall :: String -> [String]
emitCall fname = ["invokestatic " ++ fname]

compileCallInstr :: String -> CompilerData -> CompilerData
compileCallInstr fname prog = (appendBody prog (emitCall fname))
compileCallExpr
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> CallExpr
  -> CompilerData
  -> Either String CompilerData
compileCallExpr rec (CallExpr fname args) prog@(_, defs, _, _) =
  case findFunction fname (getFunctions defs) of
    Nothing -> Left ("Undefined function: " ++ fname)
    Just fdecl ->
      case checkArity fname (funcParams fdecl) args of
        Left err -> Left err
        Right () ->
          case compileArgs rec args prog of
            Left err -> Left err
            Right pArgs -> Right (compileCallInstr fname pArgs)
