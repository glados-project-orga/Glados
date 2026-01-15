module SymbolTableUtils  (getVarVal, getVarType, getVarIndex, getClassVarName) where

import CompilerTypes (CompilerData, CompilerVal(..), ShowType(..), SymInfo(..), Handle, SymbolTable)


getVar :: String -> CompilerData -> Either String SymInfo
getVar varName (_, _, _, symTable) = case lookup varName symTable of
    Just symInfo -> Right symInfo
    Nothing      -> Left ("Variable " ++ varName ++ " does not exist.")

getVarVal :: String -> CompilerData -> Either String CompilerVal
getVarVal varName prog =
    getVar varName prog >>= (\symInfo -> Right (symVal symInfo))

getVarIndex :: String -> CompilerData -> Either String Int
getVarIndex varName prog =
    getVar varName prog >>= (\symInfo -> Right (symIndex symInfo))

getVarType :: String -> CompilerData -> Either String String
getVarType varName prog = showType <$> getVarVal varName prog

getVarByHandle :: Handle -> SymbolTable-> String
getVarByHandle varHandle ((name, (SymInfo _ (ClassCmpl handle _))):elems)
    | varHandle == handle = name
    | otherwise = getVarByHandle varHandle elems
getVarByHandle varHandle (_:elems) = getVarByHandle varHandle elems
getVarByHandle _ [] = ""

getClassVarName :: Handle -> CompilerData -> String
getClassVarName varHandle (_, _, _, symTable) = getVarByHandle varHandle symTable

