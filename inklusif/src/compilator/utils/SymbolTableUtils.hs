module SymbolTableUtils  (getVarVal, getVarType) where

import CompilerTypes (CompilerData, CompilerVal(..), ShowType(..), SymInfo(..))


getVar :: String -> CompilerData -> Either String SymInfo
getVar varName (_, _, _, symTable) = case lookup varName symTable of
    Just symInfo -> Right symInfo
    Nothing      -> Left ("Variable " ++ varName ++ " does not exist.")

getVarVal :: String -> CompilerData -> Either String CompilerVal
getVarVal varName prog =
    getVar varName prog >>= (\symInfo -> Right (symVal symInfo))


getVarType :: String -> CompilerData -> Either String String
getVarType varName prog = showType <$> getVarVal varName prog
