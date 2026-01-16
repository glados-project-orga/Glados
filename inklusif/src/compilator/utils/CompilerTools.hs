module CompilerTools (
    appendHeader,
    appendDefines,
    appendBody,
    appendSymbolTable,
    storeInConstantPool,
    getTypePrefix,
    typePrefixVal,
    getLitArrayType,
    isArrayMixed,
    validAssignmentType,
    getNuancedArray,
    convertToType,
    cmplValToExpr,
    )
where

import Data.Either (lefts, rights)
import Data.Maybe (listToMaybe, fromMaybe)
import CompilerTypes(
    CompilerData,
    ConstantPool,
    Defines,
    Bytecode,
    SymbolTable,
    TypeEq(..),
    TypeNormalized(..),
    Convert(..),
    ConvertExpr(..),
    SearchTypes(..),
    )
import SymbolTableUtils (getVarType, getVarType)
import FunctionUtils (getFunctionReturnType)
import Ast (Declaration(..),
    Expr(..),
    CallExpr(..),
    MethodCallExpr(..),
    ClassDecl(..),
    StructField(..),
    Type(..),
    Literal(..),
    )
import Data.List (find)

appendHeader :: CompilerData -> ConstantPool -> CompilerData
appendHeader (header, def, body, symblTable) newHead =
    (header ++ newHead, def, body, symblTable)

appendDefine :: Declaration -> Defines -> Defines
appendDefine (Function function) (c, fun, st, en, td, count) = (c, fun ++ [function], st, en, td, count)
appendDefine (Class struct) (c, fun, st, en, td, count) = (c, fun, st ++ [struct], en, td, count)
appendDefine (Enum enum) (c, fun, st, en, td, count) = (c, fun, st, en ++ [enum], td, count)
appendDefine (Typedef typedef) (c, fun, st, en, td, count) = (c, fun, st, en, td ++ [typedef], count)

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

typePrefixVal :: Type -> String
typePrefixVal (IntType ) = "i"
typePrefixVal (LongType ) = "l"
typePrefixVal (FloatType ) = "f"
typePrefixVal (DoubleType ) = "d"
typePrefixVal (CharType ) = "c"
typePrefixVal (BoolType ) = "b"
typePrefixVal (ArrayType _) = "a"
typePrefixVal _ = "i"

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
arrayCellValidType (VarExpr varName) prog = show <$> getVarType varName prog
arrayCellValidType (ArrayVarExpr _ _) _ = Right "array"
arrayCellValidType _ _ = Left "Invalid expression type for array cell"

getClasses :: CompilerData -> [ClassDecl]
getClasses (_, (_, _, classes, _, _, _), _, _) = classes

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
normalizeExprType (VarExpr name) prog = getVarType name prog >>= (Right . TypeNorm)
normalizeExprType (ArrayVarExpr name _) prog = getVarType name prog >>= (Right . TypeNorm)
normalizeExprType (ClassVarExpr clName (VarExpr varName)) prog =
    getClassVarType clName varName prog >>= \typ -> Right (TypeNorm typ)
normalizeExprType (ArrayLiteral arr) prog =
    getLitArrayType arr prog >>= \typ -> Right (TypeNorm (convert typ))
normalizeExprType (CallExpression (CallExpr name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType (MethodCallExpression (MethodCallExpr _ name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType _ _ = Left "Unknown expression type"

normalizeToType :: TypeNormalized -> Type
normalizeToType (TypeNorm typ) = typ
normalizeToType (LitNorm lit) = convert lit

convertToType :: Expr -> CompilerData-> Either String Type
convertToType expr prog = normalizeExprType expr prog >>= (Right . normalizeToType)

validAssignmentType :: SearchTypes -> SearchTypes -> CompilerData -> Bool
validAssignmentType (SearchType t1) (SearchType t2) _ = t1 == t2
validAssignmentType (SearchExpr expr) (SearchType t) prog = t `typeEq` (convertToType expr prog)
validAssignmentType (SearchType t) (SearchExpr expr) prog = t `typeEq` (convertToType expr prog)
validAssignmentType (SearchExpr expr1) (SearchExpr expr2) prog = normed1 `typeEq` normed2
    where normed1 = convertToType expr1 prog
          normed2 = convertToType expr2 prog

cmplValToExpr :: Type -> CompilerData -> Expr
cmplValToExpr (CustomType name) _ = (LitExpr (StringLit name))
cmplValToExpr val _ = convertExpr val
