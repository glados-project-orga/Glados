module Var (compileVar) where
import CompilerTypes (CompilerData)

compileVar :: String -> CompilerData -> Either String CompilerData
compileVar _ prog = Right prog