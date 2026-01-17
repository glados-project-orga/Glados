{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr) where

import CompilerTypes (CompilerData, Search(..), CompileExpr)
import FunctionUtils (searchFunctions)
import CompilerTools (appendBody, validAssignmentType)
import Ast (Expr(..), CallExpr(..), Parameter(..), FunctionDecl(..))

pushArg :: CompileExpr -> Expr -> Parameter -> CompilerData -> Either String CompilerData
pushArg re expr (Parameter _ parType _) prog
    | validAssignmentType (srch parType) (srch expr) prog = re expr prog
    | otherwise = Left "Invalid parameter type in function call."

pushArgs :: CompileExpr -> [Expr] -> [Parameter] -> CompilerData -> Either String CompilerData
pushArgs _ (_:_) [] _ = Left "Too much arguments in function call."
pushArgs _ [] (_:_) _ = Left "Not enough arguments in function call."
pushArgs _ [] [] prog = Right prog
pushArgs re (e:es) (p:ps) prog = pushArgs re es ps prog >>= pushArg re e p

compileCallExpr:: CompileExpr -> CallExpr -> CompilerData -> Either String CompilerData
compileCallExpr re (CallExpr name exprs) prog@(_, defs, _, _) = eitherParams >>=
    \params -> pushArgs re exprs params prog >>=
    \n_prog -> Right (appendBody n_prog ["invokestatic " ++ name])
    where eitherParams = case searchFunctions name defs of
            Just (FunctionDecl _ _ foundParams _ _) -> Right (foundParams)
            Nothing -> Left ("Function " ++ name ++ " not found.")