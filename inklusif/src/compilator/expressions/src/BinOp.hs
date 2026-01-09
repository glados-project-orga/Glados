module BinOp (compileBinOp) where
import CompilerTypes (CompilerData)
import Ast (BinOp, Expr)

compileBinOp :: (BinOp, Expr, Expr) -> CompilerData -> Either String CompilerData
compileBinOp _ prog = Right prog