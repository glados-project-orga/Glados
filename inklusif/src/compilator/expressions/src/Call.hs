{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- Call.hs
-}

module Call (compileCallExpr, compileMethExpr) where

import CompilerTypes (CompilerData, Search(..), CompileExpr)
import FunctionUtils (searchFunctions)
import CompilerTools (appendBody, validAssignmentType)
import Ast (Expr(..), CallExpr(..), Parameter(..), FunctionDecl(..), Literal(..))

pushArg :: CompileExpr -> Expr -> Parameter -> CompilerData -> Either String CompilerData
pushArg re expr (Parameter _ parType _) prog
    | validAssignmentType (srch parType) (srch expr) prog = re expr prog
    | otherwise = Left "Invalid parameter type in function call."

pushArgs :: CompileExpr -> [Expr] -> [Parameter] -> CompilerData -> Either String CompilerData
pushArgs _ (_:_) [] _ = Left "Too much arguments in function call."
pushArgs _ [] (_:_) _ = Left "Not enough arguments in function call."
pushArgs _ [] [] prog = Right prog
pushArgs re (e:es) (p:ps) prog = pushArgs re es ps prog >>= pushArg re e p



compileMethExpr:: CompileExpr -> [String] -> CallExpr -> CompilerData -> Either String CompilerData
compileMethExpr re this (CallExpr name exprs) prog@(_, defs, _, _) = eitherParams >>=
    \params -> pushArgs re exprs params prog >>= \incomplete_prog -> (Right (appendBody incomplete_prog this)) >>=
    \n_prog -> Right (appendBody n_prog ["invokestatic " ++ name])
    where eitherParams = case searchFunctions name defs of
            Just (FunctionDecl _ _ foundParams _ _) -> case foundParams of
                _:thisSkip -> Right thisSkip
                _ -> Left ("Method " ++ name ++ " missing 'this' parameter.")
            Nothing -> Left ("Function " ++ name ++ " not found.")

compileCallExpr:: CompileExpr -> CallExpr -> CompilerData -> Either String CompilerData
compileCallExpr compileExpr (CallExpr "write" args) prog
    | length args /= 2 = Left "Error exepected two arguments for write"
    | otherwise =
        case args of
            (x:(LitExpr (IntLit n)): _) ->
                if n <= 0 then Left "exepected 1 or 2 for output"
                else
                    compileExpr x prog >>= \p1 ->
                    Right (appendBody p1 ["invoke_write " ++ show n])
            _ -> Left "Error: write expects (expr, int)"

compileCallExpr _ (CallExpr "open" args) prog
    | length args /= 1 =  Left "Error exepected one argument for open"
    | otherwise =
        case args of
            [LitExpr (StringLit file)] -> 
                let prog1 = appendBody prog ["sconst \"" ++ file ++ "\""] 
                    prog2 = appendBody prog1 ["invoke_open"]
                in Right prog2
            _ -> Left "Error: open expects a string literal"

compileCallExpr re (CallExpr name exprs) prog@(_, defs, _, _) = eitherParams >>=
    \params -> pushArgs re exprs params prog >>=
    \n_prog -> Right (appendBody n_prog ["invokestatic " ++ name])
    where eitherParams = case searchFunctions name defs of
            Just (FunctionDecl _ _ foundParams _ _) -> Right (foundParams)
            Nothing -> Left ("Function " ++ name ++ " not found.")