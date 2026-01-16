module ArrayLiteral (compileArrayLiteral, getLitArrayType) where
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody, getLitArrayType, isArrayMixed, getNuancedArray)
import Ast (Expr(..))
import Data.Maybe (fromMaybe, listToMaybe)

compileOneCell :: CompileExpr -> [Expr] -> Int -> CompilerData -> Either String CompilerData
compileOneCell _ [] _ prog = Right prog
compileOneCell re (ex:exs) index prog = (re ex addedIndexProg)
    >>= (\new_prog -> compileOneCell re exs (index + 1) new_prog)
        where addedIndexProg = appendBody prog [("iconst " ++ show index)]

compileArrayLiteral :: CompileExpr -> [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral _ [] prog = Right prog
compileArrayLiteral re exprs prog
    | not (null arrayError) = Left firstError
    | isArrayMixed validTypes = Left "Array contains mixed expression types."
    | otherwise = compileOneCell re exprs 0 prog
        where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
              (arrayError, validTypes) = getNuancedArray exprs prog
