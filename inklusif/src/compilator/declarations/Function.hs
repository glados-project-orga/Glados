module Function (compileFunction) where
import Ast (FunctionDecl(..))
import Statements (compileStatements)
import CompilerTypes (ProgramBinary)

compileFunction :: FunctionDecl -> ProgramBinary-> ProgramBinary
compileFunction (FunctionDecl _ name _ _ statements) (header, defs, body)
    = new_prog
        where
          new_prog = (new_head, new_defs, new_body ++ ["}\n"])
          (new_head, new_defs, new_body) = compileStatements statements (open_fun, [])
          open_fun = (header, defs, body ++ ["fun ++ " ++ name ++ " {\n"])
