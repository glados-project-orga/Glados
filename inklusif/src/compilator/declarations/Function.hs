module Function (compileFunction) where
import Ast (FunctionDecl(..))
import Statements (compileStatements)
import CompilerTypes (ProgramBinary)

compileFunction :: FunctionDecl -> ProgramBinary
compileFunction fun@(FunctionDecl pos name params return_type body) =
    ([], ["Fun " ++ name ++ " {\n" ++ compileStatements body ++ "}\n"])