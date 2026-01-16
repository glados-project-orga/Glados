module CompilerMain (compilerMain, compileAst) where
import CompilerTypes (CompilerData, Ast)
import Ast (Declaration(..))
import Function (compileFunction)
import Class (compileClass)
import Enum (compileEnum)
import Typedef (compileTypedef)
import Writer (writeBytecode)
import Labels (resolveLabels)

initialData :: CompilerData
initialData = ([], (0, [], [], [], [], 0), [], [])

compileDeclarations :: Declaration -> CompilerData -> Either String CompilerData
compileDeclarations (Function fun) (cp, def, bc, _) = compileFunction fun (cp, def, bc, [])
compileDeclarations (Class struct) prog = compileClass struct prog
compileDeclarations (Enum enum) prog = compileEnum enum prog
compileDeclarations (Typedef typedef) prog = compileTypedef typedef prog

compilerMain :: Ast -> Either String CompilerData -> Either String CompilerData
compilerMain _ (Left err) = Left err
compilerMain [] (Right prog) = Right prog
compilerMain (declaration:ast) (Right prog) = compilerMain ast new_prog
    where
        new_prog = compileDeclarations declaration prog

compileAst :: Ast -> Either String (IO ())
compileAst ast = compilerMain ast (Right initialData) >>= \prog ->
    Right (writeBytecode (resolveLabels prog)) >>= \content ->
    Right (writeFile "binary.ink" content)