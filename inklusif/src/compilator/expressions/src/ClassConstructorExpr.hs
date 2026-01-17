module ClassConstructorExpr (compileClassConstructorExpr) where
import Ast (Expr(..))
import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

compileClassConstructorExpr :: (String, [Expr]) -> CompilerData -> Either String CompilerData
compileClassConstructorExpr (cname, _) prog = Right (appendBody prog ["new " ++ cname]) 