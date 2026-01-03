module Function (compileFunction) where
import Ast (FunctionDecl(..))
import Statements (compileStatements)
import CompilerTypes (ConstantPool, Bytecode)

compileFunction :: FunctionDecl -> (ConstantPool, Bytecode)
compileFunction fun@(FunctionDecl pos name params return_type body) =
    ([], ["Fun " ++ name ++ " {\n" ++ compileStatements body ++ "}\n"])