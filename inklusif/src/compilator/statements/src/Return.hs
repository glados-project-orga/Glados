module Return (compileReturn) where
import CompilerTypes (CompilerData)
import CompilerTools (convertToType, typePrefixVal, appendBody)
import Ast (ReturnStmt(..))
import Expr (compileExpr)

compileReturn :: ReturnStmt -> CompilerData -> Either String CompilerData
compileReturn (ReturnStmt expr) prog = convertToType expr prog >>=
    \t -> compileExpr expr prog >>= \exprProg ->
    Right (appendBody exprProg [typePrefixVal t ++ "return"])