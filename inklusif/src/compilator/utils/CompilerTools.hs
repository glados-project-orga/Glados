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
    addValToHeader,
    getArraySubType,
    isArrayGivenType,
    isClassDefined
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
    ArrayVar(..),
    )
import Data.List (find)

appendHeader :: CompilerData -> ConstantPool -> CompilerData
appendHeader (header, def, body, symblTable) newHead =
    (header ++ newHead, def, body, symblTable)

addValToHeader :: CompilerData -> Expr -> Either String CompilerData
addValToHeader prog (LitExpr (IntLit i)) = Right (appendHeader prog [show i])
addValToHeader prog (LitExpr (FloatLit f)) = Right (appendHeader prog [show f])
addValToHeader prog (LitExpr (DoubleLit d)) = Right (appendHeader prog [show d])
addValToHeader prog (LitExpr (CharLit c)) = Right (appendHeader prog [show c])
addValToHeader prog (LitExpr (StringLit s)) = Right (appendHeader prog [show s])
addValToHeader prog (LitExpr (BoolLit b)) = Right (appendHeader prog [show b])
addValToHeader prog (LitExpr (LongLit l)) = Right (appendHeader prog [show l])
addValToHeader _ _ = Left "Unsupported literal type for header addition."

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
typePrefixVal _ = "a"

getNuancedArray :: [Expr] -> CompilerData -> ([String], [Type])
getNuancedArray [] _ = ([], [])
getNuancedArray exprs prog = (lefts arrayTypes, rights arrayTypes)
    where arrayTypes = map (\expr -> convertToType expr prog) exprs

isArrayGivenType :: Type -> [Type] -> Bool
isArrayGivenType VoidType [] = True
isArrayGivenType t xs = all (== t) xs

isArrayMixed :: [Type] -> Bool
isArrayMixed [] = False
isArrayMixed (headType:xs) = any (/= headType) xs

getLitArrayType :: [Expr] -> CompilerData -> Either String Type
getLitArrayType [] _ = Right (ArrayType (ArrayVar VoidType (LitExpr (IntLit 0))))
getLitArrayType exprs prog | not (null arrayError) = Left firstError
                           | isArrayMixed validTypes = Left "Array contains mixed expression types."
                           | otherwise = Right (ArrayType (ArrayVar (fval) (LitExpr (IntLit 0))))
    where firstError = fromMaybe "Unknown error" (listToMaybe arrayError)
          fval = fromMaybe VoidType (listToMaybe validTypes)
          (arrayError, validTypes) = getNuancedArray exprs prog

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

isClassDefined :: String -> Defines -> Bool
isClassDefined searched (_, _, classDefs, _, _, _) =
    any (\(ClassDecl _ name _ _) -> name == searched) classDefs

normalizeBinOp :: (Expr, Expr) -> CompilerData -> Either String TypeNormalized
normalizeBinOp (left, right) prog =
    convertToType left prog >>= \normLeft ->
    convertToType right prog >>= \normRight ->
    Right (opPriorityTable (normLeft, normRight))

opPriorityTable :: (Type, Type) -> TypeNormalized
opPriorityTable op | any (== DoubleType) op = (TypeNorm DoubleType)
                   | any (== FloatType) op = (TypeNorm FloatType)
                   | any (== LongType) op = (TypeNorm LongType)
                   | any (== IntType) op = (TypeNorm IntType)
                   | otherwise = (TypeNorm VoidType)

getArraySubType :: Type -> Either String Type
getArraySubType (ArrayType (ArrayVar t _)) = Right t
getArraySubType _ = Left "Type is not an array type when searching array type."

normalizeExprType :: Expr -> CompilerData -> Either String TypeNormalized
normalizeExprType (LitExpr lit) _ = Right (LitNorm lit)
normalizeExprType (VarExpr name) prog = getVarType name prog >>= (Right . TypeNorm)
normalizeExprType (ArrayVarExpr name _) prog = getVarType name prog
    >>= getArraySubType >>= (Right . TypeNorm)
normalizeExprType (BinOpExpr _ l r) prog = normalizeBinOp (l, r) prog
normalizeExprType (CastExpr t _) _ = Right (TypeNorm t)
normalizeExprType (ClassVarExpr clName (VarExpr varName)) prog =
    getClassVarType clName varName prog >>= \typ -> Right (TypeNorm typ)
normalizeExprType (ArrayLiteral arr) prog =
    getLitArrayType arr prog >>= \typ -> Right (TypeNorm typ)
normalizeExprType (CallExpression (CallExpr name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType (MethodCallExpression (MethodCallExpr _ name _)) prog =
    case getFunctionReturnType name prog of
        Just retType -> Right (TypeNorm retType)
        Nothing -> Left "Unknown function return type"
normalizeExprType expr _ = Left ("Unknown expression type :" ++ (show expr))

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