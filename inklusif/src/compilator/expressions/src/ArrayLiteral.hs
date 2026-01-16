module ArrayLiteral (compileArrayLiteral, getLitArrayType) where
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody, getLitArrayType, isArrayMixed, getNuancedArray, typePrefixVal)
import Ast (Expr(..), Type(..))
import Data.Maybe (fromMaybe, listToMaybe)

compileOneCell :: CompileExpr -> [Expr] -> Int -> String -> CompilerData -> Either String CompilerData
compileOneCell _ [] _ _ prog = Right prog
compileOneCell re (ex:exs) index prefix prog = (re ex addedIndexProg)
    >>= (\cellProg -> Right (appendBody cellProg [prefix ++ "astore"]))
    >>= (\new_prog -> compileOneCell re exs (index + 1) prefix new_prog)
        where addedIndexProg = appendBody prog ["dup", ("iconst " ++ show index)]

compileArrayLiteral :: CompileExpr -> [Expr] -> Type -> CompilerData -> Either String CompilerData
compileArrayLiteral _ [] _ prog = Right prog
compileArrayLiteral re exprs t prog
    | not (null arrayError) = Left firstError
    | isArrayMixed validTypes = Left "Array contains mixed expression types."
    | otherwise = compileOneCell re exprs 0 prefix prog
        where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
              prefix = typePrefixVal t
              (arrayError, validTypes) = getNuancedArray exprs prog
