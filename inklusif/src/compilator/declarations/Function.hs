module Function (compileFunction) where
import ast AST (FunctionDecl(..))

compileFunction :: FunctionDecl -> (ConstantPool, Bytecode)
compileFunction _ _ _ _ _ = ([], [])