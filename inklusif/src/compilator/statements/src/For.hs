module For (compileFor) where

import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Ast (ForStmt(..), ForUpdate(..), Statement(..))
import Expr (compileExpr)
import Labels (generateLabel)


compileFor :: (Statement -> CompilerData -> Either String CompilerData) 
        -> ForStmt
        -> CompilerData
        -> Either String CompilerData 

compileFor compileStmt (ForStmt initStmt condExpr updateStmt body) prog = 
    
    compileInit initStmt prog >>= \prog1 ->
    
    let (lStart, prog2) = generateLabel prog1
        (lEnd,   prog3) = generateLabel prog2
        prog4 = addLd (lStart ++ ":") prog3
    in

    compileExpr condExpr prog4 >>= \prog5 ->
    
    let prog6 = addLd ("ifeq " ++ lEnd) prog5
    in
    compileBody body prog6 >>= \prog7 ->
    compileUpdate updateStmt prog7 >>= \prog8 ->

    let prog9 = addLd ("goto " ++ lStart) prog8
        prog10 = addLd (lEnd ++ ":") prog9
    in
    Right prog10

    where
        compileInit Nothing p = Right p
        compileInit (Just s) p = compileStmt s p
        
        compileBody [] p = Right p 
        compileBody (s:ss) p =
            compileStmt s p >>= \p' -> 
            compileBody ss p'
        
        compileUpdate (ForUpdateExpr e) p = compileExpr e p
        compileUpdate (ForUpdateStmt s) p = compileStmt s p 
        
        addLd instr p = appendBody p [instr]

