module ForEach (compileForEach) where
import CompilerTypes (CompilerData)
import Ast (ForEachStmt(..), Statement(..))
import Labels (generateLabel)
import CompilerTools (appendBody)
import Expr (compileExpr)


compileForEach :: (Statement -> CompilerData -> Either String CompilerData)
            -> ForEachStmt
            -> CompilerData
            -> Either String CompilerData

compileForEach compileStmt (ForEachStmt var collection body) prog =

    compileExpr collection prog >>= \prog1 ->
    let prog2 = addLd "store col" prog1
    in

    let prog3 = addLd "push 0" prog2
        prog4 = addLd "store i" prog3
    in

    let (lStart, prog5) = generateLabel prog4
        (lEnd,   prog6) = generateLabel prog5
        prog7 = addLd (lStart ++ ":") prog6
    in

    let prog8  = addLd "load i" prog7
        prog9  = addLd "load col" prog8
        prog10 = addLd "size" prog9
        prog11 = addLd ("if_icmpge " ++ lEnd) prog10
    in

    let prog12 = addLd "load col" prog11
        prog13 = addLd "load i" prog12
        prog14 = addLd "get" prog13
        prog15 = addLd ("store " ++ var) prog14
    in

    compileBody body prog15 >>= \prog16 ->

    let prog17 = addLd "load i" prog16
        prog18 = addLd "push 1" prog17
        prog19 = addLd "add" prog18
        prog20 = addLd "store i" prog19
    in

    let prog21 = addLd ("goto " ++ lStart) prog20
        prog22 = addLd (lEnd ++ ":") prog21
    in
    Right prog22

  where
    compileBody [] p = Right p
    compileBody (s:ss) p =
        compileStmt s p >>= \p' ->
        compileBody ss p'

    addLd instr p = appendBody p [instr]
