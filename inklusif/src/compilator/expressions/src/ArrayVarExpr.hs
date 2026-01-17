module ArrayVarExpr (compileArrayVarExpr) where

import CompilerTools (typePrefixVal, appendBody, getArraySubType)
import SymbolTableUtils (getVar)
import CompilerTypes (CompilerData, SymInfo(..), CompileExpr)
import Ast (Expr(..))

compileArrayVarExpr :: CompileExpr -> (String, Expr) -> CompilerData -> Either String CompilerData
compileArrayVarExpr re (varName, indexExprs) prog = arrVar
    >>= \(idx, t) -> Right (appendBody prog ["aload " ++ show idx])
    >>= \loadProg -> re indexExprs loadProg
    >>= \idxProg  -> Right (appendBody idxProg ([typePrefixVal t ++ "aload" ]))
        where arrVar = getVar varName prog
                        >>= \(SymInfo idx t _ _) -> getArraySubType t
                        >>= \subType -> Right (idx, subType)