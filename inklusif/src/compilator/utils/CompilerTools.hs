module CompilerTools (
    appendHeader,
    appendDefines,
    appendBody) where
import CompilerTypes(ProgramBinary, ConstantPool, Defines, Bytecode)
import Ast (Declaration(..))

appendHeader :: ProgramBinary -> ConstantPool -> ProgramBinary
appendHeader (header, def, body) newHead = (header ++ newHead, def, body)

appendDefine :: Declaration -> Defines -> Defines
appendDefine (Function function) (fun, st, en, td) = (fun ++ [function], st, en, td)
appendDefine (Struct struct) (fun, st, en, td) = (fun, st ++ [struct], en, td)
appendDefine (Enum enum) (fun, st, en, td) = (fun, st, en ++ [enum], td)
appendDefine (Typedef typedef) (fun, st, en, td) = (fun, st, en, td ++ [typedef])

appendDefines :: ProgramBinary -> [Declaration] -> ProgramBinary
appendDefines prog [] = prog
appendDefines (header, def, body) (newDef:decl) = appendDefines new_prog decl
    where new_prog =  (header, appendDefine newDef def, body)

appendBody :: ProgramBinary -> Bytecode -> ProgramBinary
appendBody (header, def, body) newBody = (header, def, body ++ newBody)