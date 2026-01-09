{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Compile (compileCallExpr) where

import Ast (Expr(..), CallExpr(..), FunctionDecl(..))
import CompilerTypes (CompilerData)
import Call (compileCallInstr)

import Lookup (getFunctions, findFunction)
import Arity (checkArity)
import ExprArgs (compileArgs)

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
