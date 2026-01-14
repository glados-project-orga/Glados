module FunctionUtils (
    getFunctions,
    findFunction,
    searchFunctions,
    getFunctionReturnType
  ) where
import CompilerTypes (Defines)
import Ast (FunctionDecl(..), Type(..))

getFunctions :: Defines -> [FunctionDecl]
getFunctions (_, funs, _, _, _) = funs

getFunctionReturnType :: FunctionDecl -> Type
getFunctionReturnType (FunctionDecl _ _ _ retType _) = retType

findFunction :: String -> [FunctionDecl] -> Maybe FunctionDecl
findFunction _ [] = Nothing
findFunction name (f:fs)
  | funcName f == name = Just f
  | otherwise          = findFunction name fs

searchFunctions :: String -> Defines -> Maybe FunctionDecl
searchFunctions name defs = findFunction name (getFunctions defs)