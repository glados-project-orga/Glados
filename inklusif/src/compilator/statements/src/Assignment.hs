module Assignment (compileAssignment) where
import CompilerTypes (CompilerData)
import CompilerTools (appendBody, validAssignmentType)
import SymbolTableUtils (getVarIndex)
import Expr (compileExpr)
import Ast (Assignment(..), Expr(..))

compileAssignment :: Assignment -> CompilerData -> Either String CompilerData
compileAssignment (Assignment target@(VarExpr name) value) prog |
     validAssignmentType target value prog =
        compileExpr value prog >>= \value_prog -> getVarIndex name value_prog >>=
        \varIndex -> Right (appendBody value_prog ["astore " ++ show varIndex])
    | otherwise = Left "Invalid assignment types."
compileAssignment _ _ = Left "Unknown assignment target."
