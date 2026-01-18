module Assignment (compileAssignment) where
import CompilerTypes (CompilerData, Search(..), SearchTypes(..))
import CompilerTools (appendBody, validAssignmentType, typePrefixVal, convertToType, getTypePrefix, getArraySubType)
import SymbolTableUtils (getVarIndex, getVarType)
import Expr (compileExpr)
import Ast (Assignment(..), Expr(..), ArrayIndexExpr(..), Type(..))


validExprForTask :: SearchTypes -> SearchTypes -> CompilerData -> Either String CompilerData
validExprForTask val sexpr@(SearchExpr expr) prog |
    validAssignmentType val sexpr prog = compileExpr expr prog
    | otherwise = Left ("Invalid expression type for array index."
        ++ show (convertToType expr prog) ++ "." ++ " Expected " ++ show val ++ ".")
validExprForTask  _ _ _ = Left "Invalid search type for array index."

compileArrayIndex :: ArrayIndexExpr -> CompilerData -> Either String CompilerData
compileArrayIndex  (ArrayIndexExpr name index val) prog =
    getVarIndex name prog >>= \varIndex ->
    Right (appendBody prog ["aload " ++ show varIndex]) >>= \loadProg ->
    validExprForTask (srch IntType) (srch index) loadProg >>= \indexProg ->
    getVarType name indexProg >>= \arrayVal -> getArraySubType arrayVal >>= \subType ->
    validExprForTask (srch subType) (srch val) indexProg >>=
    \valProg -> Right $ appendBody valProg [(getTypePrefix (show subType)) ++ "astore"]

basicAssignment  :: Assignment -> CompilerData -> Either String CompilerData
basicAssignment (Assignment target@(VarExpr name) value) prog
    | validAssignmentType (srch target) (srch value) prog =
        compileExpr (value) prog >>= \value_prog -> getVarIndex name value_prog >>=
        \varIndex -> getVarType name value_prog >>= \varVal -> Right (appendBody value_prog 
        [typePrefixVal varVal ++ "store " ++ show varIndex])
    | otherwise = Left ("Invalid assignment types for " ++ name ++ ": " ++ (show (convertToType value prog)) ++ ".")
basicAssignment target _ = Left ("Unknown assignment target. " ++ show target)

classAssignment :: Expr -> Expr -> CompilerData -> Either String CompilerData
classAssignment target@(ClassVarExpr objName (VarExpr name)) value prog |
    validAssignmentType (srch target) (srch value) prog =
        getVarIndex objName prog >>=
        \objIndex -> (Right (appendBody prog ["aload " ++ show objIndex])) >>= 
        \objProg -> compileExpr value objProg >>=
        \valueProg -> Right $ appendBody valueProg ["putfield " ++ name]
    | otherwise = Left ("Invalid assignment types for field " ++ name ++ ": " ++
        (show (convertToType value prog)) ++ " " ++ name ++ " being "
        ++ show (convertToType target prog) ++ ".")
classAssignment _ _ _ = Left "Unknown assignment target."

compileAssignment :: Assignment -> CompilerData -> Either String CompilerData
compileAssignment (Assignment target@(VarExpr _) value) prog = basicAssignment (Assignment target value) prog
compileAssignment (Assignment (ArrayVarExpr arrName indexExpr) value) prog =
    compileArrayIndex (ArrayIndexExpr arrName indexExpr value) prog
compileAssignment (Assignment target@(ClassVarExpr _ (VarExpr _)) value) prog =
    classAssignment target value prog
compileAssignment _ _ = Left "Unknown assignment target." 
