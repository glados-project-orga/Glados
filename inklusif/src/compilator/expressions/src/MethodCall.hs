module MethodCall (compileMethodCall) where

import CompilerTypes (CompilerData, Search(..))
import FunctionUtils (searchFunctions)
import CompilerTools (appendBody, validAssignmentType)
import Ast (Expr(..), MethodCallExpr(..), Parameter(..), FunctionDecl(..))
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

compileMethodCall:: MethodCallExpr -> CompilerData -> Either String CompilerData
compileMethodCall (MethodCallExpr obj name exprs) prog@(_, defs, _, _) = eitherParams >>=
    \params -> pushArgs (obj:exprs) params prog >>=
    \n_prog -> Right (appendBody n_prog ["invokstatic " ++ name])
    where eitherParams = case searchFunctions name defs of
            Just (FunctionDecl _ _ foundParams _ _) -> Right (foundParams)
            Nothing -> Left ("Function " ++ name ++ " not found.")