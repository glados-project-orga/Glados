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
import Builtins(apply)

import qualified Data.Map as Map
type Env = Map.Map String Ast

evalAST :: Env -> Ast -> Either String (Env, Ast)
evalAST env (AInt li) = Right (env, AInt li)
evalAST env (ABool bool) =  Right (env, ABool bool)
evalAST env (ALambda args body) = Right (env, ALambda args body)

evalAST env (ADefine name expr) =
  case evalAST env expr of
    Right(_, val) -> Right(Map.insert name val env, AVoid)
    Left err -> Left err

evalAST env (ASymbol symb) =
  case Map.lookup symb env of
    Just val -> Right (env, val)
    Nothing -> Left ("Unbound symbol: " ++ symb)


evalAST env (ACall (ASymbol name) args) =

  case Map.lookup name apply of
    Just f ->
      case traverse (fmap snd . evalAST env) args of
        Left err -> Left err
        Right vals ->
          case f vals of
            Left err -> Left err
            Right val -> Right (env, val)
  
    Nothing ->
        case Map.lookup name env of
          Just (ALambda argss body) ->
            if length argss == length args
              then case traverse (fmap snd . evalAST env) args of
                Left err     -> Left err
                Right argvals ->
                  let newEnv = Map.union (Map.fromList (zip argss argvals)) env
                  in evalAST newEnv body
              else Left "Invalid number of arguments"

          Just other ->
            Left ("Trying to call non-function: " ++ show other)

          Nothing ->
            Left ("Unknown function: " ++ name)

evalAST env (ACall ffn args) =

  case evalAST env ffn of
    Left err -> Left err

    Right(_, ALambda argss body) ->
      if length argss == length args
        then case traverse (fmap snd . evalAST env) args of
          Left err -> Left err
          Right argvals ->
            let newEnv = Map.union(Map.fromList (zip argss argvals)) env
            in evalAST newEnv body
      else Left "Invalid number of arguments"

    Right(_, ASymbol name) ->
      case Map.lookup name apply of
        Nothing -> Left ("Unknown function: " ++ name)
        Just f ->
          case traverse (fmap snd . evalAST env) args of
            Left err -> Left err
            Right vals ->
              case f vals of
                Left err -> Left err
                Right val -> Right (env, val)

    Right (_, other) ->
      Left ("Trying to call non-function: " ++ show other)

evalAST _ baned = Left ("No correct Ast format: " ++ show baned)
