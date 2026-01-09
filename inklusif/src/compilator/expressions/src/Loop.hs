module Loop (compileLoop) where
import CompilerTypes (CompilerData)
import Ast (LoopBranch, LoopResult)

compileLoop :: [LoopBranch] -> LoopResult -> CompilerData -> Either String CompilerData
compileLoop _ _ prog = Right prog