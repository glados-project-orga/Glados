module FunctionUtils (
    getFunctions,
    findFunction,
    searchFunctions,
    getFunctionReturnType
  ) where
import CompilerTypes (Defines, CompilerData)
import Ast (FunctionDecl(..), Type(..))

getFunctions :: Defines -> [FunctionDecl]
getFunctions (_, funs, _, _, _, _) = funs

getFunctionReturnType :: String -> CompilerData -> Maybe Type
getFunctionReturnType name (_, def, _, _) = function >>= Just . funcReturnType
  where function = (findFunction name (getFunctions def))


findFunction :: String -> [FunctionDecl] -> Maybe FunctionDecl
findFunction _ [] = Nothing
findFunction name (f:fs)
  | funcName f == name = Just f
  | otherwise          = findFunction name fs

searchFunctions :: String -> Defines -> Maybe FunctionDecl
searchFunctions name defs = findFunction name (getFunctions defs)