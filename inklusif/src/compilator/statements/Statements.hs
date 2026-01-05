module Statements (compileStatements) where
import Ast (Statement(..),
    VarDecl(..),
    Assignment(..),
    IfStmt(..),
    WhileStmt(..),
    ForStmt(..),
    ForEachStmt(..),
    MatchStmt(..),
    TryCatchStmt(..),
    ThrowStmt(..),
    ExprStmt(..)
    )
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

compileStatement :: Statement -> String
compileStatement (VarDeclStmt vr_dcl) = compileVarDecl vr_dcl
compileStatement (AssignmentStmt assign) = compileAssignment assign
compileStatement (IfStatement if_st) = compileIf if_st
compileStatement (WhileStatement while) = compileWhile while
compileStatement (ForStatement for) = compileFor for
compileStatement (ForEachStatement for_each) = compileForEach for_each
compileStatement (MatchStatement match) = compileMatch match
compileStatement (TryCatchStatement tryCatch) = compileTryCatch tryCatch
compileStatement (ThrowStatement throw) = compileThrow throw
compileStatement (ExprStatement expr) = compileExpr expr
compileStatement _ = ""

compileStatements :: [Statement] -> String
compileStatements stmts = concatMap compileStatement stmts