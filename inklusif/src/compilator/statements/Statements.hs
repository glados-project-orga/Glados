module Statements (compileStatements) where
import Ast (Statement(..),)
import CompilerTypes (ProgramBinary)
import VarDecl (compileVarDecl)
import Assignment (compileAssignment)
import If (compileIf)
import While (compileWhile)
import For (compileFor)
import ForEach (compileForEach)
import Match (compileMatch)
import TryCatch (compileTryCatch)
import Throw (compileThrow)
import Expr (compileExpr)
import CompilerTools (appendProgramBinaries)

compileStatement :: Statement -> ProgramBinary -> ProgramBinary
compileStatement (VarDeclStmt vr_dcl) prog = compileVarDecl vr_dcl prog
compileStatement (AssignmentStmt assign) prog = compileAssignment assign prog
compileStatement (IfStatement if_st) prog = compileIf if_st prog
compileStatement (WhileStatement while) prog = compileWhile while prog
compileStatement (ForStatement for) prog = compileFor for prog
compileStatement (ForEachStatement for_each) prog = compileForEach for_each prog
compileStatement (MatchStatement match) prog = compileMatch match prog
compileStatement (TryCatchStatement tryCatch) prog = compileTryCatch tryCatch prog
compileStatement (ThrowStatement throw) prog = compileThrow throw prog
compileStatement (ExprStatement expr) prog = compileExpr expr prog
compileStatement _ _ = ([], [], [])

compileStatements :: [Statement] -> ProgramBinary -> ProgramBinary
compileStatements [] prog = prog
compileStatements (stmt:stmts) prog = compileStatements stmts n_prog
    where
        n_prog = appendProgramBinaries prog (compileStatement stmt prog)