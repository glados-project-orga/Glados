module Typedef (compileTypedef) where
import Ast (TypedefDecl(..), Declaration(..))
import CompilerTypes (CompilerData)
import CompilerError (errPos)
import CompilerTools (appendDefines)

typeNameExists :: String -> [TypedefDecl] -> Bool
typeNameExists searched typedefs =
    any (\(TypedefDecl _ _ name) -> name == searched) typedefs

compileTypedef :: TypedefDecl  -> CompilerData -> Either String CompilerData
compileTypedef typedef@(TypedefDecl pos _ name) prog@(_, (_, _, _, typedefs), _, _)
    | typeNameExists name typedefs =
        Left ((errPos pos) ++ "Typedef " ++ name ++ " is already defined.")
    | otherwise = Right (appendDefines prog [(Typedef typedef)])