{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr) where

import CompilerTypes (CompilerData, Search(..))
import FunctionUtils (searchFunctions)
import CompilerTools (appendBody, validAssignmentType)
import Ast (Expr(..), CallExpr(..), Parameter(..), FunctionDecl(..))
import Expr (compileExpr)

pushArg :: Expr -> Parameter -> CompilerData -> Either String CompilerData
pushArg expr (Parameter _ parType _) prog
    | validAssignmentType (srch parType) (srch expr) prog = compileExpr expr prog
    | otherwise = Left "Invalid parameter type in function call."

pushArgs :: [Expr] -> [Parameter] -> CompilerData -> Either String CompilerData
pushArgs (_:_) [] _ = Left "Too much arguments in function call."
pushArgs [] (_:_) _ = Left "Not enough arguments in function call."
pushArgs [] [] prog = Right prog
pushArgs (e:es) (p:ps) prog = pushArgs es ps prog >>= pushArg e p

compileCallExpr:: CallExpr -> CompilerData -> Either String CompilerData
compileCallExpr (CallExpr name exprs) prog@(_, defs, _, _) = eitherParams >>=
    \params -> pushArgs exprs params prog >>=
    \n_prog -> Right (appendBody n_prog ["invokstatic " ++ name])
    where eitherParams = case searchFunctions name defs of
            Just (FunctionDecl _ _ foundParams _ _) -> Right (foundParams)
            Nothing -> Left ("Function " ++ name ++ " not found.")