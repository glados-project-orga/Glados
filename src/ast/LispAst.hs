module LispAst (
  getSymbol,
  getList,
  getInterger,
  sexprToAST
) where

import Data.Traversable()
import Data.Maybe()
import Builtins (apply)
import Data(Ast(..), SExpr(..))

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol mp) = Just mp
getSymbol _            = Nothing

getInterger :: SExpr -> Maybe Int
getInterger (SInt mp) = Just mp
getInterger _         = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList mp) = Just mp
getList  _         = Nothing

pretty :: SExpr -> String
pretty (SSymbol stt) = "a Symbol '" ++ stt ++ "'"
pretty (SInt ii)     = "a Number " ++ show ii
pretty (SList [])    = "an empty List"
pretty (SList (x:xs)) =
  "a List with " ++ pretty x ++
  concatMap (\e -> " followed by " ++ pretty e) xs

printTree :: SExpr -> Maybe String
printTree expr = Just (pretty expr)


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt mp) = Just (AInt mp)
sexprToAST (SSymbol ll) = Just (ASymbol ll)

sexprToAST (SList [SSymbol "define", SSymbol name, expr]) =
  case sexprToAST expr of
    Just astExpr -> Just (ADefine name astExpr)
    Nothing      -> Nothing

sexprToAST (SList (SSymbol "define" : _)) = Nothing

sexprToAST (SList (fnExpr:args)) = 
  case sexprToAST fnExpr of
    Just fnAst -> case traverse sexprToAST args of
                    Just argAsts -> Just (ACall fnAst argAsts)
                    Nothing      -> Nothing
    Nothing -> Nothing

sexprToAST (SList mpp) =
  case traverse sexprToAST mpp of
    Just asts -> Just (AList asts)
    Nothing   -> Nothing