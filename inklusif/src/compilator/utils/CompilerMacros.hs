module CompilerMacros (onlyBody) where
import CompilerTypes (CompilerData)

onlyBody :: [String] -> CompilerData
onlyBody code = ([], ([], [], [], []), code, [])