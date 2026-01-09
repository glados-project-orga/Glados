{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Expr (compileExpr) where

import Ast
  ( Expr(..)
  , Literal(..)
  , BinOp(..)
  , CallExpr(..)
  , FunctionDecl(..)
  , Parameter(..)
  )
import CompilerTypes (ProgramLayer, ProgramBinary, SymbolTable)
import CompilerTools (appendBody)
import Call (compileCallInstr)

-- petit helper pour éviter de répéter left right left right left right partout
bindE :: Either String a -> (a -> Either String b) -> Either String b
bindE (Left err) _ = Left err
bindE (Right x)  f = f x

addBody :: [String] -> ProgramLayer -> ProgramLayer
addBody bc (bin, st) = (appendBody bin bc, st)


emitLoadVar :: String -> SymbolTable -> Either String [String]
emitLoadVar name st =
  case lookup name st of
    Nothing  -> Left ("Undefined variable: " ++ name)
    Just idx -> Right [emitLoadIdx idx]

emitLoadIdx :: Int -> String
emitLoadIdx 0 = "iload_0"
emitLoadIdx 1 = "iload_1"
emitLoadIdx 2 = "iload_2"
emitLoadIdx 3 = "iload_3"
emitLoadIdx n = "iload " ++ show n

getFunctions :: ProgramBinary -> [FunctionDecl]
getFunctions (_, (funs, _, _, _), _) = funs

findFunction :: String -> [FunctionDecl] -> Maybe FunctionDecl
findFunction _ [] = Nothing
findFunction name (f:fs)
  | funcName f == name = Just f
  | otherwise          = findFunction name fs

checkArity :: String -> [Parameter] -> [Expr] -> Either String ()
checkArity fname params args
  | length params == length args = Right ()
  | otherwise                    = Left ("wrong number of arguments for " ++ fname)

emitPushInt :: Int -> [String]
emitPushInt (-1) = ["iconst_m1"]
emitPushInt 0    = ["iconst_0"]
emitPushInt 1    = ["iconst_1"]
emitPushInt 2    = ["iconst_2"]
emitPushInt 3    = ["iconst_3"]
emitPushInt 4    = ["iconst_4"]
emitPushInt 5    = ["iconst_5"]
emitPushInt n
  | n >= (-128) && n <= 127       = ["bipush " ++ show n]
  | n >= (-32768) && n <= 32767   = ["sipush " ++ show n]
  | otherwise                     = ["ldc " ++ show n]

emitBinOp :: BinOp -> Either String [String]
emitBinOp Add = Right ["iadd"]
emitBinOp Sub = Right ["isub"]
emitBinOp Mul = Right ["imul"]
emitBinOp Div = Right ["idiv"]
emitBinOp Mod = Right ["irem"]
emitBinOp op  = Left ("BinOp not implemented yet: " ++ show op)

compileArgs :: [Expr] -> ProgramLayer -> Either String ProgramLayer
compileArgs [] layer = Right layer
compileArgs (e:es) layer =
  bindE (compileExpr e layer) (\layer' -> compileArgs es layer')

compileCall :: CallExpr -> ProgramLayer -> Either String ProgramLayer
compileCall (CallExpr fname args) layer@(bin, _) =
  compileCallChecked (findFunction fname (getFunctions bin)) fname args layer

compileCallChecked :: Maybe FunctionDecl -> String -> [Expr] -> ProgramLayer -> Either String ProgramLayer
compileCallChecked Nothing fname _ _ =
  Left ("Undefined function: " ++ fname)
compileCallChecked (Just fdecl) fname args layer =
  bindE (checkArity fname (funcParams fdecl) args) (\_ ->
    bindE (compileArgs args layer) (\layerArgs ->
      Right (compileCallInstr fname layerArgs)
    )
  )

compileExpr :: Expr -> ProgramLayer -> Either String ProgramLayer
compileExpr (LitExpr (IntLit n)) layer =
  Right (addBody (emitPushInt n) layer)

compileExpr (VarExpr name) (bin, st) =
  bindE (emitLoadVar name st) (\bc -> Right (addBody bc (bin, st)))

compileExpr (BinOpExpr op a b) layer =
  bindE (compileExpr a layer) (\layer1 ->
    bindE (compileExpr b layer1) (\layer2 ->
      bindE (emitBinOp op) (\bc ->
        Right (addBody bc layer2)
      )
    )
  )

compileExpr (CallExpression call) layer =
  compileCall call layer

compileExpr _ _ =
  Left "Expression not implemented yet"
