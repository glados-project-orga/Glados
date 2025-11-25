{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Eval.hs
-}

module Eval (
    evalAST
) where

import Data(Ast(..))
import Builtins(apply)

evalAST :: Ast -> Maybe Ast
evalAST (AInt li) = Just (AInt li)
evalAST (ASymbol _) = Nothing
evalAST (ACall (ASymbol fn) args) =
  case traverse evalAST args of
    Just vals -> apply fn vals
    Nothing   -> Nothing
evalAST (AList mp) = Just (AList mp)
evalAST _ = Nothing
