module Typedef (compileTypedef, getTypedefType, typeNameExists) where
import Ast (TypedefDecl(..), Declaration(..), Type(..))
import CompilerTypes (CompilerData)
import CompilerError (errPos)
import CompilerTools (appendDefines)
import Data.List (find)

typeNameExists :: String -> [TypedefDecl] -> Bool
typeNameExists searched typedefs =
    any (\(TypedefDecl _ _ name) -> name == searched) typedefs

getTypedefType :: String -> [TypedefDecl] -> Maybe Type
getTypedefType searched typedefs =
    typedefOriginal <$> find (\td -> typedefAlias td == searched) typedefs

compileTypedef :: TypedefDecl  -> CompilerData -> Either String CompilerData
compileTypedef typedef@(TypedefDecl pos _ name) prog@(_, (_, _, _, _, typedefs, _), _, _)
    | typeNameExists name typedefs =
        Left ((errPos pos) ++ "Typedef " ++ name ++ " is already defined.")
    | otherwise = Right (appendDefines prog [(Typedef typedef)])