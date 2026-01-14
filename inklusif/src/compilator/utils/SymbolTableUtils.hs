module SymbolTableUtils  (getVarVal, getVarType) where

import CompilerTypes (CompilerData, CompilerVal(..), ShowType(..), SymInfo(..))

getVarVal :: String -> CompilerData -> Either String CompilerVal
getVarVal varName (_, _, _, symTable) = case lookup varName symTable of
    Just symInfo -> Right (symVal symInfo)
    Nothing      -> Left ("Variable " ++ varName ++ " does not exist.")

getVarType :: String -> CompilerData -> Either String String
getVarType varName prog = showType <$> getVarVal varName prog
