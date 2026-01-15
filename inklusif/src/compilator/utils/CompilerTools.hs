module CompilerTools (
    appendHeader,
    appendDefines,
    appendBody,
    appendSymbolTable,
    storeInConstantPool,
    getTypePrefix,
    getLitArrayType,
    isArrayMixed,
    validAssignmentType,
    getNuancedArray
    ) where
import Data.Either (lefts, rights)
import Data.Maybe (listToMaybe, fromMaybe)
import CompilerTypes(CompilerData,
    ConstantPool,
    Defines,
    Bytecode,
    SymbolTable,
    TypeEq(..),
    CompilerVal(..),
    ShowType(..)
    )
import Data.Either (fromRight)
import SymbolTableUtils (getVarVal, getVarType)
import FunctionUtils (getFunctionReturnType, getFunctions, findFunction)
import Ast (Declaration(..),
    Expr(..),
    CallExpr(..),
    MethodCallExpr(..)
    )

appendHeader :: CompilerData -> ConstantPool -> CompilerData
appendHeader (header, def, body, symblTable) newHead =
    (header ++ newHead, def, body, symblTable)

appendDefine :: Declaration -> Defines -> Defines
appendDefine (Function function) (c, fun, st, en, td) = (c, fun ++ [function], st, en, td)
appendDefine (Class struct) (c, fun, st, en, td) = (c, fun, st ++ [struct], en, td)
appendDefine (Enum enum) (c, fun, st, en, td) = (c, fun, st, en ++ [enum], td)
appendDefine (Typedef typedef) (c, fun, st, en, td) = (c, fun, st, en, td ++ [typedef])

appendDefines :: CompilerData -> [Declaration] -> CompilerData
appendDefines prog [] = prog
appendDefines (header, def, body, symblTable) (newDef:decl) = appendDefines new_prog decl
    where new_prog = (header, appendDefine newDef def, body, symblTable)

appendBody :: CompilerData -> Bytecode -> CompilerData
appendBody (header, def, body, symblTable) newBody =
    (header, def, body ++ newBody, symblTable)

appendSymbolTable :: CompilerData -> SymbolTable -> CompilerData
appendSymbolTable (header, def, body, symblTable) newSymblTable =
    (header, def, body, symblTable ++ newSymblTable)

storeInConstantPool :: CompilerData -> String -> (CompilerData, Int)
storeInConstantPool prog@(header, _, _, _) newElem = (appendHeader prog [newElem], length header)

getTypePrefix :: String -> String
getTypePrefix "int" = "i"
getTypePrefix "float" = "f"
getTypePrefix "double" = "d"
getTypePrefix "char" = "c"
getTypePrefix "bool" = "b"
getTypePrefix _ = "a"

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

arrayCellValidType :: Expr -> CompilerData -> Either String String
arrayCellValidType (ArrayLiteral arr) prog = getLitArrayType arr prog
arrayCellValidType (VarExpr varName) prog = getVarType varName prog
arrayCellValidType (ArrayVarExpr _ _) _ = Right "array"
arrayCellValidType _ _ = Left "Invalid expression type for array cell"

validAssignmentType :: CompilerVal -> Expr -> CompilerData -> Bool
validAssignmentType val (VarExpr name) prog = either (const False) (val `typeEq`) (getVarVal name prog)
validAssignmentType val (ArrayVarExpr name _) prog = showType val == fromRight "" (getVarType name prog)
validAssignmentType val (ClassVarExpr _ (VarExpr varName)) prog = showType val == fromRight "" (getVarType varName prog)
validAssignmentType val (LitExpr lit) _= val `typeEq` lit
validAssignmentType val (ArrayLiteral arr) prog = showType val == fromRight "" (getLitArrayType arr prog)
validAssignmentType val (CallExpression (CallExpr name _)) (_, def, _, _) =
    maybe False (\func -> val `typeEq` getFunctionReturnType func) (findFunction name (getFunctions def))
validAssignmentType val (MethodCallExpression (MethodCallExpr _ name _)) (_, def, _, _) =
    maybe False (\func -> val `typeEq` getFunctionReturnType func) (findFunction name (getFunctions def))
validAssignmentType _ _ _ = False