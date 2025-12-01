{- 
-- EPITECH PROJECT, 2025
-- epi-repo
-- File description:
-- Eval.hs
-}

module Eval (
    evalAST,
    Env
) where

import Data(Ast(..))
import Builtins (apply)

import qualified Data.Map as Map
type Env = Map.Map String Ast

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

evalAST env (ACall (ASymbol name) args) =
  case Map.lookup name apply of
    Nothing -> Left ("Unknown function: " ++ name)
    Just f ->
      case traverse (fmap snd . evalAST env) args of
        Left err -> Left err
        Right vals ->
          case f vals of
            Left err -> Left err
            Right val -> Right (env, val)

evalAST _ baned = Left ("No correct Ast format" ++ show baned)
