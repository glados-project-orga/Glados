{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr, compileCallExprT) where

import Ast (Expr(..),
            CallExpr(..),
            FunctionDecl(..),
            Type(..),
            Parameter(..))
import CompilerTypes (CompilerData)
import Lookup (getFunctions, findFunction)
import Arity (checkArity)
import ExprArgs (compileArgs, compileArgsT)
import CompilerTools (appendBody)
import TypeCheck (expectType)
import EitherUtils (bindE, thenE)

emitCall :: String -> [String]
emitCall fname = ["invokestatic " ++ fname]

compileCallInstr :: String -> CompilerData -> CompilerData
compileCallInstr fname prog = appendBody prog (emitCall fname)

compileCallExpr
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> CallExpr
  -> CompilerData
  -> Either String CompilerData
compileCallExpr rec (CallExpr fname args) prog@(_, defs, _, _) =
  compileCallFound rec fname args prog (findFunction fname (getFunctions defs))

compileCallFound
  :: (Expr -> CompilerData -> Either String CompilerData)
  -> String -> [Expr] -> CompilerData
  -> Maybe FunctionDecl
  -> Either String CompilerData
compileCallFound _ fname _ _ Nothing = Left ("Undefined function: " ++ fname)
compileCallFound rec fname args prog (Just fdecl) =
  thenE (checkArity fname (funcParams fdecl) args)
    (bindE (compileArgs rec args prog) (\pArgs ->
      Right (compileCallInstr fname pArgs)))

compileCallExprT
  :: (Expr -> CompilerData -> Either String (Type, CompilerData))
  -> CallExpr
  -> CompilerData
  -> Either String (Type, CompilerData)
compileCallExprT rec (CallExpr fname args) prog@(_, defs, _, _) =
  compileCallFoundT rec fname args prog (findFunction fname (getFunctions defs))

compileCallFoundT
  :: (Expr -> CompilerData -> Either String (Type, CompilerData))
  -> String -> [Expr] -> CompilerData
  -> Maybe FunctionDecl
  -> Either String (Type, CompilerData)
compileCallFoundT _ fname _ _ Nothing = Left ("Undefined function: " ++ fname)
compileCallFoundT rec fname args prog (Just fdecl) =
  let expected = map paramType (funcParams fdecl)
      retT     = funcReturnType fdecl
  in thenE (checkArity fname (funcParams fdecl) args)
       (bindE (compileArgsT rec args prog) (\(given, pArgs) ->
         thenE (checkArgTypes fname expected given)
           (Right (retT, compileCallInstr fname pArgs))))

checkArgTypes :: String -> [Type] -> [Type] -> Either String ()
checkArgTypes fname expected given = prefixErr ("Type error in call to " ++ fname ++ ": ") (checkPairs 1 expected given)

checkPairs :: Int -> [Type] -> [Type] -> Either String ()
checkPairs _ [] [] = Right ()
checkPairs i (e:es) (g:gs) = thenE (prefixArg i (expectType e g)) (checkPairs (i + 1) es gs)
checkPairs _ _ _ = Left "arity mismatch (unexpected)"

prefixArg :: Int -> Either String () -> Either String ()
prefixArg _ ok@(Right ()) = ok
prefixArg i (Left err) = Left ("argument " ++ show i ++ " " ++ err)

prefixErr :: String -> Either String a -> Either String a
prefixErr _ ok@(Right _) = ok
prefixErr p (Left e) = Left (p ++ e)
