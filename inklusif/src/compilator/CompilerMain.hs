module CompilerMain (compilerMain) where
import CompilerTypes (CompilerData, Ast)
import Ast (Declaration(..))
import Function (compileFunction)
import Class (compileClass)
import Enum (compileEnum)
import Typedef (compileTypedef)

compileDeclarations :: Declaration -> CompilerData -> Either String CompilerData
compileDeclarations (Function fun) prog = compileFunction fun prog
compileDeclarations (Class struct) prog = compileClass struct prog
compileDeclarations (Enum enum) prog = compileEnum enum prog
compileDeclarations (Typedef typedef) prog = compileTypedef typedef prog



compilerMain :: Ast -> Either String CompilerData -> Either String CompilerData
compilerMain _ (Left err) = Left err
compilerMain [] (Right prog) = Right prog
compilerMain (declaration:ast) (Right prog) = compilerMain ast new_prog
    where
        new_prog = compileDeclarations declaration prog