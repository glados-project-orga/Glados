module Assignment (compileAssignment) where
import CompilerTypes (CompilerData, Search(..))
import CompilerTools (appendBody, validAssignmentType, typePrefixVal, convertToType)
import SymbolTableUtils (getVarIndex, getVarType)
import Expr (compileExpr)
import Ast (Assignment(..), Expr(..))

compileAssignment :: Assignment -> CompilerData -> Either String CompilerData
compileAssignment (Assignment target@(VarExpr name) value) prog |
     validAssignmentType (srch target) (srch value) prog =
        compileExpr (value) prog >>= \value_prog -> getVarIndex name value_prog >>=
        \varIndex -> getVarType name value_prog >>= \varVal -> Right (appendBody value_prog 
        [typePrefixVal varVal ++ "store " ++ show varIndex])
    | otherwise = Left ("Invalid assignment types for " ++ name ++ ": " ++ (show (convertToType value prog)) ++ ".")
compileAssignment _ _ = Left "Unknown assignment target."
