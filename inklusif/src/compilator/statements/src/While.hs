module While (compileWhile) where

import CompilerTools (appendBody)
import CompilerTypes (CompilerData)
import Ast (WhileStmt(..), Statement(..))
import Labels (generateLabel)
import Expr (compileExpr)


compileWhile :: (Statement -> CompilerData -> Either String CompilerData)
        -> WhileStmt 
        -> CompilerData 
        -> Either String CompilerData
compileWhile compileStmt (WhileStmt cond body) prog =

    let (lStart, prog1) = generateLabel prog
        (lEnd,   prog2) = generateLabel prog1
        prog3 = addLd (lStart ++ ":") prog2
    in
    
    compileExpr cond prog3 >>= \prog4 ->

    let prog5 = addLd ("ifeq " ++ lEnd) prog4
    in
    
    compileBody body prog5 >>= \prog6 ->

    let prog7 = addLd ("goto " ++ lStart) prog6
        prog8 = addLd (lEnd ++ ":") prog7
    in
    Right prog8
    
    where 
        compileBody [] p = Right p 
        compileBody (s:ss) p = 
            compileStmt s p >>= \p' -> 
            compileBody ss p'
        addLd instr p = appendBody p [instr]