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
    getNuancedArray,
    convertToCompilerVal
    )
where

import Data.Either (lefts, rights)
import Data.Maybe (listToMaybe, fromMaybe)
import CompilerTypes(CompilerData,
    ConstantPool,
    Defines,
    Bytecode,
    SymbolTable,
    TypeEq(..),
    CompilerVal(..),
    TypeNormalized(..),
    Convert(..)
    )
import SymbolTableUtils (getVarType, getVarVal)
import FunctionUtils (getFunctionReturnType)
import Ast (Declaration(..),
    Expr(..),
    CallExpr(..),
    MethodCallExpr(..),
    ClassDecl(..),
    StructField(..),
    Type(..),
    )
import Data.List (find)

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

getClasses :: CompilerData -> [ClassDecl]
getClasses (_, (_, _, classes, _, _), _, _) = classes

getClass :: String -> CompilerData -> Either String ClassDecl
getClass clname prog = 
    maybe (Left ("Class " ++ clname ++ " does not exist."))
          Right (find (\(ClassDecl _ name _ _) -> name == clname) classes)
    where classes = getClasses prog

getClassVarType ::  String -> String -> CompilerData -> Either String Type
getClassVarType clname varName prog =
    getClass clname prog >>= \(ClassDecl _ _ fields _) ->
    maybe (Left ("Variable " ++ varName ++ " does not exist in class " ++ clname ++ "."))
          (Right . structFieldType) (find (\(StructField name _) -> name == varName) fields)

normalizeExprType :: Expr -> CompilerData -> Either String TypeNormalized
normalizeExprType (LitExpr lit) _ = Right (LitNorm lit)
normalizeExprType (VarExpr name) prog = getVarVal name prog >>= (Right . CmplNorm)
normalizeExprType (ArrayVarExpr name _) prog = getVarVal name prog >>= (Right . CmplNorm)
normalizeExprType (ClassVarExpr clName (VarExpr varName)) prog =
    getClassVarType clName varName prog >>= \typ -> Right (TypeNorm typ)
normalizeExprType (ArrayLiteral arr) prog =
    getLitArrayType arr prog >>= \typ -> Right (CmplNorm (convert typ))
normalizeExprType (CallExpression (CallExpr name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType (MethodCallExpression (MethodCallExpr _ name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType _ _ = Left "Unknown expression type"

normalizeToCmpl :: TypeNormalized -> CompilerVal
normalizeToCmpl (CmplNorm val) = val
normalizeToCmpl (TypeNorm typ) = convert typ
normalizeToCmpl (LitNorm lit) = convert lit

convertToCompilerVal :: Expr -> CompilerData-> Either String CompilerVal
convertToCompilerVal expr prog = normalizeExprType expr prog >>= (Right . normalizeToCmpl)

validAssignmentType :: Expr -> Expr -> CompilerData -> Bool
validAssignmentType expr1 expr2 prog = normed1 `typeEq` normed2
    where normed1 = convertToCompilerVal expr1 prog
          normed2 = convertToCompilerVal expr2 prog

-- isSameType :: CompilerVal -> Expr -> CompilerData -> Bool
-- isSameType val (VarExpr name) prog = val `typeEq` (getVarType name prog)
-- isSameType val (ArrayVarExpr name _) prog = val `typeEq` (getVarType name prog)
-- isSameType val (ClassVarExpr _ (VarExpr varName)) prog = val `typeEq` (getVarType varName prog)
-- isSameType val (LitExpr lit) _= val `typeEq` lit
-- isSameType val (ArrayLiteral arr) prog = val `typeEq` (getLitArrayType arr prog)
-- isSameType val (CallExpression (CallExpr name _)) prog =
--     val `typeEq` getFunctionReturnType name prog
-- isSameType val (MethodCallExpression (MethodCallExpr _ name _)) prog =
--     val `typeEq` getFunctionReturnType name prog
-- isSameType _ _ _ = False


-- validAssignmentType val (VarExpr name) prog = either (const False) (val `typeEq`) (getVarVal name prog)
