module ArrayLiteral (compileArrayLiteral) where
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Expr (compileExpr)
import Ast (Expr)

compileOneCell :: [Expr] -> Int -> CompilerData -> Either String CompilerData
compileOneCell [] _ prog = Right prog
compileOneCell (ex:exs) index prog = compileExpr ex addedIndexProg
    >>= (\new_prog -> compileOneCell exs (index + 1) new_prog)
        where addedIndexProg = appendBody prog [("iconst " ++ show index)]

compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral [] prog = Right prog
compileArrayLiteral exprs prog = compileOneCell exprs 0 prog