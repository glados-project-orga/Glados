module Function (compileFunction) where
import Ast (FunctionDecl(..))
import Statements (compileStatements)
import CompilerTypes (ProgramBinary)

closeFunction :: Either String ProgramBinary -> Either String ProgramBinary
closeFunction (Left err) = Left err
closeFunction (Right (new_head, new_defs, new_body)) =
    Right (new_head, new_defs, new_body ++ ["}\n"])

compileFunction :: FunctionDecl -> ProgramBinary-> Either String ProgramBinary
compileFunction (FunctionDecl _ name _ _ statements) (header, defs, body)
    = new_prog
        where
          new_prog = closeFunction uncomplete_prog
          uncomplete_prog = compileStatements statements (Right (open_fun, []))
          open_fun = (header, defs, body ++ ["fun ++ " ++ name ++ " {\n"])
