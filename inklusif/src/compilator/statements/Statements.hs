module Statements (compileStatements) where
import Ast (Statement(..), ExprStmt(..))
import CompilerTypes (CompilerData, CompilerData)
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

compileStatement :: Statement -> CompilerData -> Either String CompilerData
compileStatement (VarDeclStmt vr_dcl) layer = compileVarDecl vr_dcl layer
compileStatement (AssignmentStmt assign) layer = compileAssignment assign layer
compileStatement (IfStatement if_st) layer = compileIf if_st layer
compileStatement (WhileStatement while) layer = compileWhile while layer
compileStatement (ForStatement for) layer = compileFor for layer
compileStatement (ForEachStatement for_each) layer = compileForEach for_each layer
compileStatement (MatchStatement match) layer = compileMatch match layer
compileStatement (TryCatchStatement tryCatch) layer = compileTryCatch tryCatch layer
compileStatement (ThrowStatement throw) layer = compileThrow throw layer
compileStatement (ExprStatement (ExprStmt expr)) layer = compileExpr expr layer
compileStatement _ _ = Left "Unsupported statement type in compileStatement"

compileStatements :: [Statement] -> Either String CompilerData -> Either String CompilerData
compileStatements _ (Left err) = Left err
compileStatements [] (Right prog) = Right prog
compileStatements (stmt:stmts) (Right layer) = compileStatements stmts n_layer
    where
        n_layer = compileStatement stmt layer