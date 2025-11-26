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
-- import Builtins (apply)

import qualified Data.Map as Map
type Env = Map.Map String Ast

-- evalAST :: Ast -> Maybe Ast
-- evalAST (AInt li) = Just (AInt li)
-- evalAST (ASymbol _) = Nothing
-- evalAST (ACall (ASymbol fn) args) =
--   case traverse evalAST args of
--     Just vals -> apply fn vals
--     Nothing   -> Nothing
-- evalAST (AList mp) = Just (AList mp)
-- evalAST _ = Nothing

evalAST :: Env -> Ast -> Either String (Env, Ast)
evalAST env (AInt li) = Right (env, AInt li)

evalAST env (ASymbol symb) =
  case Map.lookup symb env of
    Just val -> Right (env, val)
    Nothing -> Left ("Unbound symbol: " ++ symb)

evalAST env (ADefine name expr) =
  case evalAST env expr of
    Right(_, val) -> Right(Map.insert name val env, val)
    Left err -> Left err

-- evalAST env (ACall (ASymbol name) args) =


evalAST _ baned = Left ("No correct Ast formar" ++ show baned)