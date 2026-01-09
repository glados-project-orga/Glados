module UnaryOp (compileUnaryOp) where
import CompilerTypes (CompilerData)
import Ast (Expr)

compileUnaryOp :: String -> Expr -> CompilerData -> Either String CompilerData
compileUnaryOp _ _ prog = Right prog