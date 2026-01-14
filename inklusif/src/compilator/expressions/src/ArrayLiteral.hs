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

compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral [] prog = Right prog
compileArrayLiteral exprs prog = compileOneCell exprs 0 prog