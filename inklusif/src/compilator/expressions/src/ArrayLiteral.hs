module ArrayLiteral (compileArrayLiteral, getLitArrayType) where
import CompilerTypes (CompilerData)
import CompilerTools (appendBody, getLitArrayType, isArrayMixed, getNuancedArray)
import Expr (compileExpr)
import Ast (Expr(..))
import Data.Maybe (fromMaybe, listToMaybe)

compileOneCell :: [Expr] -> Int -> CompilerData -> Either String CompilerData
compileOneCell [] _ prog = Right prog
compileOneCell (ex:exs) index prog = compileExpr ex addedIndexProg
    >>= (\new_prog -> compileOneCell exs (index + 1) new_prog)
        where addedIndexProg = appendBody prog [("iconst " ++ show index)]

compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral [] prog = Right prog
compileArrayLiteral exprs prog
    | not (null arrayError) = Left firstError
    | isArrayMixed validTypes = Left "Array contains mixed expression types."
    | otherwise = compileOneCell exprs 0 prog
        where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
              (arrayError, validTypes) = getNuancedArray exprs prog
