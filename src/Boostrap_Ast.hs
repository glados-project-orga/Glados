module Boostrap_Ast(Ast(..), SExpr(..), getSymbol, getList, getInterger, sexprToAST) where
import Data.Traversable()
import Data.Maybe()


data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)

data Ast = AInt Int
         | ASymbol String
         | AList [Ast]
         | ACall Ast [Ast]
         | ADefine String Ast
         deriving (Show, Eq)


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

apply :: String -> [Ast] -> Maybe Ast
apply "+" [AInt a, AInt b] = Just (AInt (a + b))
apply "-" [AInt a, AInt b] = Just (AInt (a - b))
apply "*" [AInt a, AInt b] = Just (AInt (a * b))
apply _ _ = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (AInt li) = Just (AInt li)
evalAST (ASymbol _) = Nothing
evalAST (ACall (ASymbol fn) args) =
  case traverse evalAST args of
    Just vals -> apply fn vals
    Nothing   -> Nothing
evalAST (AList mp) = Just (AList mp)
evalAST _ = Nothing


-- main :: IO ()
-- main = do

  -- print (sexprToAST (SInt 5))
  -- print (sexprToAST (SSymbol "foo"))
  -- print (sexprToAST(SList[SInt 5, SSymbol "symbol"]))

  -- print (sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 5]))
  -- print (sexprToAST (SList [SSymbol "define", SSymbol "x", SInt 5, SInt 6]))
  -- print(sexprToAST (SList [SSymbol "+", SSymbol "x", SInt 4]))

  -- print(sexprToAST (SList [SSymbol "+", SSymbol "x", SList [SSymbol "*", SInt 4, SSymbol "y"]]))
  -- print(sexprToAST (SList [SSymbol "define", SSymbol "fourtyTwo", SList [SSymbol "*", SInt 7, SInt 6]]))
  -- print(evalAST(AList [ASymbol "define", ASymbol "x", AInt 5]))