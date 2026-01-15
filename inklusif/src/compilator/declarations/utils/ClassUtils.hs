module ClassUtils (getClassVarType) where
import Ast (ClassDecl(..), StructField(..), Type(..))
import CompilerTypes (CompilerData)
import Data.List (find)

getClasses :: CompilerData -> [ClassDecl]
getClasses (_, (_, _, classes, _, _), _, _) = classes

getClass :: String -> CompilerData -> Either String ClassDecl
getClass clname prog = 
    maybe (Left ("Class " ++ clname ++ " does not exist."))
          Right (find (\(ClassDecl _ name _ _) -> name == clname) classes)
    where classes = getClasses prog

getClassVarType ::  String -> String -> CompilerData -> Either String Type
getClassVarType clname varName prog =
    getClass clname prog >>= \(ClassDecl _ _ fields _) ->
    maybe (Left ("Variable " ++ varName ++ " does not exist in class " ++ clname ++ "."))
          (Right . structFieldType) (find (\(StructField name _) -> name == varName) fields)