module Statements (compileStatements, compileStatement, manageBody) where

import Ast (Statement(..), ExprStmt(..), Type(..), ReturnStmt(..))
import CompilerTools (validAssignmentType, convertToType)
import CompilerTypes (CompilerData, Search(..))
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
import Return (compileReturn)
import CompileBlock (compileBlock)

compileStatement :: Statement -> CompilerData -> Either String CompilerData
compileStatement (VarDeclStmt vr_dcl) layer = compileVarDecl vr_dcl layer
compileStatement (AssignmentStmt assign) layer = compileAssignment assign layer
compileStatement (IfStatement if_st) layer = compileIf compileStatement if_st layer
compileStatement (WhileStatement while) layer = compileWhile compileStatement while layer
compileStatement (ForStatement for) layer = compileFor compileStatement for layer
compileStatement (ForEachStatement for_each) layer = compileForEach compileStatement for_each layer
compileStatement (MatchStatement match) layer = compileMatch match layer
compileStatement (TryCatchStatement tryCatch) layer = compileTryCatch tryCatch layer
compileStatement (ThrowStatement throw) layer = compileThrow throw layer
compileStatement (ExprStatement (ExprStmt expr)) layer = compileExpr expr layer
compileStatement (ReturnStatement ret) layer = compileReturn ret layer
compileStatement _ _ = Left "Unsupported statement type in compileStatement"

checkReturnType :: [Statement] -> Type -> CompilerData -> Either String CompilerData
checkReturnType [] VoidType prog = Right (prog)
checkReturnType [] _ _ = Left "Function missing return statement in non-void function."
checkReturnType ((ReturnStatement (ReturnStmt expr)):_) retype prog@(_, header, _, _)
    | validAssignmentType (srch retype) (srch expr) prog = Right (prog)
    | otherwise = Left ("Invalid return Type " ++ (show (convertToType expr prog)) ++ (show (header))  ++ " in function.")
checkReturnType (_:stmts) retype prog = checkReturnType stmts retype prog

compileStatements :: [Statement] ->  Either String CompilerData -> Either String CompilerData
compileStatements _ (Left err) = Left err
compileStatements stmts (Right layer) =
  compileBlock compileStatement stmts layer

manageBody :: [Statement] -> Type -> CompilerData -> Either String CompilerData
manageBody stmts t prog = compileStatements stmts (Right prog)
  >>= \newBodyProg -> checkReturnType stmts t newBodyProg
