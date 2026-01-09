module CompilerTools (
    appendHeader,
    appendDefines,
    appendBody,
    appendSymbolTable
    ) where
import CompilerTypes(CompilerData, ConstantPool, Defines, Bytecode, SymbolTable)
import Ast (Declaration(..))

appendHeader :: CompilerData -> ConstantPool -> CompilerData
appendHeader (header, def, body, symblTable) newHead =
    (header ++ newHead, def, body, symblTable)

appendDefine :: Declaration -> Defines -> Defines
appendDefine (Function function) (fun, st, en, td) = (fun ++ [function], st, en, td)
appendDefine (Struct struct) (fun, st, en, td) = (fun, st ++ [struct], en, td)
appendDefine (Enum enum) (fun, st, en, td) = (fun, st, en ++ [enum], td)
appendDefine (Typedef typedef) (fun, st, en, td) = (fun, st, en, td ++ [typedef])

appendDefines :: CompilerData -> [Declaration] -> CompilerData
appendDefines prog [] = prog
appendDefines (header, def, body, symblTable) (newDef:decl) = appendDefines new_prog decl
    where new_prog = (header, appendDefine newDef def, body, symblTable)

appendBody :: CompilerData -> Bytecode -> CompilerData
appendBody (header, def, body, symblTable) newBody =
    (header, def, body ++ newBody, symblTable)

appendSymbolTable :: CompilerData -> SymbolTable -> CompilerData
appendSymbolTable (header, def, body, symblTable) newSymblTable =
    (header, def, body, symblTable ++ newSymblTable)