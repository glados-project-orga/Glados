module ArrayLiteral (compileArrayLiteral) where
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Expr (compileExpr)
import SymbolTableUtils (getVarType)
import Data.Either (lefts, rights)
import Data.Maybe (listToMaybe, fromMaybe)
import Ast (Expr(..))

arrayCellValidType :: Expr -> CompilerData ->Either String String
arrayCellValidType (ArrayLiteral arr) prog = getLitArrayType arr prog
arrayCellValidType (VarExpr varName) prog = getVarType varName prog
arrayCellValidType (ArrayVarExpr _ _) _ = Right "array"
arrayCellValidType _ _ = Left "Invalid expression type for array cell"

compileOneCell :: [Expr] -> Int -> CompilerData -> Either String CompilerData
compileOneCell [] _ prog = Right prog
compileOneCell (ex:exs) index prog = compileExpr ex addedIndexProg
    >>= (\new_prog -> compileOneCell exs (index + 1) new_prog)
        where addedIndexProg = appendBody prog [("iconst " ++ show index)]

getNuancedArray :: [Expr] -> CompilerData -> ([String], [String])
getNuancedArray [] _ = ([], [])
getNuancedArray exprs prog = (lefts arrayTypes, rights arrayTypes)
    where arrayTypes = map (\expr -> arrayCellValidType expr prog) exprs

isArrayMixed :: [String] -> Bool
isArrayMixed [] = False
isArrayMixed (headType:xs) = any (/= headType) xs

getLitArrayType :: [Expr] -> CompilerData -> Either String String
getLitArrayType [] _ = Right "array void"
getLitArrayType exprs prog | not (null arrayError) = Left firstError
                           | isArrayMixed validTypes = Left "Array contains mixed expression types."
                           | otherwise = Right ("array " ++ firstType)
    where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
          firstType = fromMaybe "unknown" (listToMaybe validTypes)
          (arrayError, validTypes) = getNuancedArray exprs prog


compileArrayLiteral :: [Expr] -> CompilerData -> Either String CompilerData
compileArrayLiteral [] prog = Right prog
compileArrayLiteral exprs prog
    | not (null arrayError) = Left firstError
    | isArrayMixed validTypes = Left "Array contains mixed expression types."
    | otherwise = compileOneCell exprs 0 prog
        where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
              (arrayError, validTypes) = getNuancedArray exprs prog