module ArrayLiteral (compileArrayLiteral, getLitArrayType) where
import CompilerTypes (CompilerData, CompileExpr)
import CompilerTools (appendBody,
    getLitArrayType,
    typePrefixVal,
    getNuancedArray,
    isArrayGivenType
    )
import Ast (Expr(..), Type(..))

compileOneCell :: CompileExpr -> [Expr] -> Int -> String -> CompilerData -> Either String CompilerData
compileOneCell _ [] _ _ prog = Right prog
compileOneCell re (ex:exs) index prefix prog = (re ex addedIndexProg)
    >>= (\cellProg -> Right (appendBody cellProg [prefix ++ "astore"]))
    >>= (\new_prog -> compileOneCell re exs (index + 1) prefix new_prog)
        where addedIndexProg = appendBody prog ["dup", ("iconst " ++ show index)]


compileArrayLiteral :: CompileExpr -> [Expr] -> Type -> CompilerData -> Either String CompilerData
compileArrayLiteral _ [] _ prog = Right prog
compileArrayLiteral re exprs t prog 
    | not (null arrayError) = Left (unlines arrayError)
    | not (isArrayGivenType t validTypes) = Left "Array literal contains invalid types."
    | otherwise = compileOneCell re exprs 0 prefix prog
        where prefix = typePrefixVal t
              (arrayError, validTypes) = getNuancedArray exprs prog

