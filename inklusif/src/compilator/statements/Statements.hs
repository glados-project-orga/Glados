module Statements (compileStatements) where
import Ast (Statement(..))

compileStatement :: Statement -> String
compileStatement _ = "/* statement */\n"

compileStatements :: [Statement] -> String
compileStatements stmts = concatMap compileStatement stmts