module SymbolTableUtils  (getVarType, getVarIndex, getVar) where
import CompilerTypes (CompilerData, SymInfo(..))
import Ast (Type(..))


getVar :: String -> CompilerData -> Either String SymInfo
getVar varName (_, _, _, symTable) = case lookup varName symTable of
    Just symInfo -> Right symInfo
    Nothing      -> Left ("Variable " ++ varName ++ " does not exist.")

getVarType :: String -> CompilerData -> Either String Type
getVarType varName prog =
    getVar varName prog >>= (\symInfo -> Right (symVal symInfo))

getVarIndex :: String -> CompilerData -> Either String Int
getVarIndex varName prog =
    getVar varName prog >>= (\symInfo -> Right (symIndex symInfo))



