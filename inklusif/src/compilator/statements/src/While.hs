module While (compileWhile, compileCondAsBranch) where

import CompilerTools (appendBody)
import CompilerTypes (CompilerData)
import Ast (WhileStmt(..), Statement(..), Expr(..), BinOp(..))
import Labels (generateLabel)
import Expr (compileExpr)


emitBranch :: String -> Expr -> Expr -> String -> CompilerData -> Either String CompilerData
emitBranch op left right lEnd prog =
    compileExpr left prog >>= \p1 ->
    compileExpr right p1 >>= \p2 ->
    Right $ appendBody p2 ["if_icmp" ++ op ++ " " ++ lEnd]


compileCondAsBranch :: Expr -> String -> CompilerData -> Either String CompilerData
compileCondAsBranch (BinOpExpr op left right) lEnd prog =
    case op of
        LessEqual     -> emitBranch "gt" left right lEnd prog
        LessThan      -> emitBranch "ge" left right lEnd prog
        GreaterEqual  -> emitBranch "lt" left right lEnd prog
        GreaterThan   -> emitBranch "le" left right lEnd prog
        Equal         -> emitBranch "ne" left right lEnd prog
        NotEqual      -> emitBranch "eq" left right lEnd prog
        _             -> Left "Condition non supportée dans un while"

compileCondAsBranch _ _ _ = Left "Condition invalide dans un while"


compileWhile :: (Statement -> CompilerData -> Either String CompilerData)
             -> WhileStmt 
             -> CompilerData 
             -> Either String CompilerData

compileWhile compileStmt (WhileStmt cond body) prog =

    let (lStart, prog1) = generateLabel prog
        (lEnd,   prog2) = generateLabel prog1
    in

    let prog3 = appendBody prog2 [lStart ++ ":"] in

    compileCondAsBranch cond lEnd prog3 >>= \prog4 ->

    compileBody body prog4 >>= \prog5 ->

    let prog6 = appendBody prog5 ["goto " ++ lStart] in

    let prog7 = appendBody prog6 [lEnd ++ ":"] in

    Right prog7

  where
    compileBody [] p = Right p
    compileBody (s:ss) p =
        compileStmt s p >>= \p' ->
        compileBody ss p'
