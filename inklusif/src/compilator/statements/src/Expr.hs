module Expr (compileExpr) where
import CompilerTypes (ProgramBinary)
import Ast (ExprStmt)

compileExpr :: ExprStmt -> ProgramBinary -> ProgramBinary
compileExpr _ prog = prog