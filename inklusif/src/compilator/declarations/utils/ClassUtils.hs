module ClassUtils (getClassVarType) where
import Ast (Declaration(..), ClassDecl(..), StructField(..), Type(..))
import CompilerTypes (CompilerData)
import Data.List (find)
import Data.Maybe (maybe)

getClasses :: CompilerData -> [ClassDecl]
getClasses (_, (_, _, classes, _, _), _, _) = classes

getClass :: String -> CompilerData -> Either String ClassDecl
getClass className prog = 
    maybe (Left ("Class " ++ className ++ " does not exist."))
          Right (find (\(ClassDecl _ name _ _) -> name == className) classes)
    where classes = getClasses prog

getClassVarType ::  String -> String -> CompilerData -> Either String Type
getClassVarType className varName prog =
    getClass className prog >>= \(ClassDecl _ _ fields _) ->
    maybe (Left ("Variable " ++ varName ++ " does not exist in class " ++ className ++ "."))
          (Right . structFieldType) (find (\(StructField name _) -> name == varName) fields)