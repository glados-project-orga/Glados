module CompilerMacros (onlyBody) where
import CompilerTypes (ProgramBinary)

onlyBody :: [String] -> ProgramBinary
onlyBody code = ([], [], code)