module ArrayLiteral (compileArrayLiteral) where
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Expr (compileExpr)
import Ast (Expr(..))

arrayCellValidType :: Expr -> Maybe String
arrayCellValidType (LitExpr _) = Just "Lit"
arrayCellValidType (VarExpr _) = Just "Var"
arrayCellValidType (ArrayVarExpr _ _) = Just "ArrayVar"
arrayCellValidType (BinOpExpr _ _ _) = Just "BinOp"
arrayCellValidType (ArrayLiteral _) = Just "ArrayLiteral"
arrayCellValidType _ = Nothing

compileOneCell :: [Expr] -> Int -> CompilerData -> Either String CompilerData
compileOneCell [] _ prog = Right prog
compileOneCell (ex:exs) index prog = compileExpr ex addedIndexProg
    >>= (\new_prog -> compileOneCell exs (index + 1) new_prog)
        where addedIndexProg = appendBody prog [("iconst " ++ show index)]

isArrayMixed :: [Maybe String] -> Bool
isArrayMixed [] = False
isArrayMixed (headType:xs) = any (/= headType) xs

compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral [] prog = Right prog
compileArrayLiteral exprs prog
    | any (== Nothing) arrayTypes = Left "Array contains invalid expression types."
    | isArrayMixed arrayTypes = Left "Array contains mixed expression types."
    | otherwise = compileOneCell exprs 0 prog
        where arrayTypes = map arrayCellValidType exprs