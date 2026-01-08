module Expr (compileExpr) where
import CompilerTypes (ProgramLayer)
import Ast (ExprStmt)

compileExpr :: ExprStmt -> ProgramLayer -> ProgramLayer
compileExpr _ prog = prog