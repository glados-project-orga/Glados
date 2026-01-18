module Assignment (compileAssignment) where
import CompilerTypes (CompilerData, Search(..))
import CompilerTools (appendBody, validAssignmentType, typePrefixVal, convertToType)
import SymbolTableUtils (getVarIndex, getVarType)
import Expr (compileExpr)
import Ast (Assignment(..), Expr(..))

basicAssignment  :: Assignment -> CompilerData -> Either String CompilerData
basicAssignment (Assignment target@(VarExpr name) value) prog
    | validAssignmentType (srch target) (srch value) prog =
        compileExpr (value) prog >>= \value_prog -> getVarIndex name value_prog >>=
        \varIndex -> getVarType name value_prog >>= \varVal -> Right (appendBody value_prog 
        [typePrefixVal varVal ++ "store " ++ show varIndex])
    | otherwise = Left ("Invalid assignment types for " ++ name ++ ": " ++ (show (convertToType value prog)) ++ ".")
basicAssignment _ _ = Left "Unknown assignment target."

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
compileAssignment (Assignment target@(ClassVarExpr _ (VarExpr _)) value) prog =
    classAssignment target value prog
compileAssignment _ _ = Left "Unknown assignment target."
