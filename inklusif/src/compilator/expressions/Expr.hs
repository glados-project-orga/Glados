module Expr (compileExpr) where
import CompilerTypes (ProgramLayer)
import Ast (Expr(..))

compileExpr :: Expr -> ProgramLayer -> ProgramLayer
compileExpr _ prog = prog