module CompilerTools (
    appendHeader,
    appendDefines,
    appendBody,
    appendSymbolTable,
    storeInConstantPool
    ) where
import CompilerTypes(CompilerData, ConstantPool, Defines, Bytecode, SymbolTable)
import Ast (Declaration(..))

appendHeader :: CompilerData -> ConstantPool -> CompilerData
appendHeader (header, def, body, symblTable) newHead =
    (header ++ newHead, def, body, symblTable)

appendDefine :: Declaration -> Defines -> Defines
appendDefine (Function function) (c, fun, st, en, td, count) = (c, fun ++ [function], st, en, td, count)
appendDefine (Class struct) (c, fun, st, en, td, count) = (c, fun, st ++ [struct], en, td, count)
appendDefine (Enum enum) (c, fun, st, en, td, count) = (c, fun, st, en ++ [enum], td, count)
appendDefine (Typedef typedef) (c, fun, st, en, td, count) = (c, fun, st, en, td ++ [typedef], count)

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

storeInConstantPool :: CompilerData -> String -> (CompilerData, Int)
storeInConstantPool prog@(header, _, _, _) newElem = (appendHeader prog [newElem], length header)