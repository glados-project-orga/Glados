module For (compileFor) where

import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Ast (ForStmt(..), ForUpdate(..), Statement(..))
import Expr (compileExpr)
import Labels (generateLabel)
import While (compileCondAsBranch)

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

    compileCondAsBranch condExpr lEnd prog4 >>= \prog5 ->

    compileBody body prog5 >>= \prog6 ->

    compileUpdate updateStmt prog6 >>= \prog7 ->

    let prog8  = addLd ("goto " ++ lStart) prog7
        prog9  = addLd (lEnd ++ ":") prog8
    in
    Right prog9

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
