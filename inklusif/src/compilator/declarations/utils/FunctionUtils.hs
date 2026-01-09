module FunctionUtils where
import CompilerTypes (CompilerData, Defines, SymbolTable)
import Ast (FunctionDecl(..))

getFunctions :: Defines -> [FunctionDecl]
getFunctions (funs, _, _, _) = funs

findFunction :: String -> [FunctionDecl] -> Maybe FunctionDecl
findFunction _ [] = Nothing
findFunction name (f:fs)
  | funcName f == name = Just f
  | otherwise          = findFunction name fs

searchFunctions :: String -> Defines -> Maybe FunctionDecl
searchFunctions name defs = findFunction name (getFunctions defs)