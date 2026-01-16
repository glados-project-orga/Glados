-- module ForEach (compileForEach) where
-- import CompilerTypes (CompilerData)
-- import Ast (ForEachStmt)


-- compileForEach :: (Statement -> CompilerData -> Either String CompilerData)
--             -> ForEachStmt
--             -> CompilerData
--             -> Either String CompilerData
-- compileForEach compileStmt (ForEachStmt var collection body) prog =

--     compileExpr collection prog >>= \prog1 ->
--     let prog2 = addLd "store _col" prog1
--     in

   
--     let prog3 = addLd "push 0" prog2
--         prog4 = addLd "store _i" prog3
--     in

--     -- labels
--     let (lStart, prog5) = generateLabel prog4
--         (lEnd,   prog6) = generateLabel prog5
--         prog7 = addLd (lStart ++ ":") prog6
--     in

--     -- tester si _i >= size(_col)
--     let prog8  = addLd "load _i" prog7
--         prog9  = addLd "load _col" prog8
--         prog10 = addLd "size" prog9
--         prog11 = addLd "ge" prog10
--         prog12 = addLd ("ifeq " ++ lEnd) prog11
--     in

--     -- charger _col[_i] dans var
--     let prog13 = addLd "load _col" prog12
--         prog14 = addLd "load _i" prog13
--         prog15 = addLd "get" prog14
--         prog16 = addLd ("store " ++ var) prog15
--     in

--     -- compiler le body
--     compileBody body prog16 >>= \prog17 ->

--     -- i++
--     let prog18 = addLd "load _i" prog17
--         prog19 = addLd "push 1" prog18
--         prog20 = addLd "add" prog19
--         prog21 = addLd "store _i" prog20
--     in

--     -- boucle
--     let prog22 = addLd ("goto " ++ lStart) prog21
--         prog23 = addLd (lEnd ++ ":") prog22
--     in
--     Right prog23

--   where
--     compileBody [] p = Right p
--     compileBody (s:ss) p =
--         compileStmt s p >>= \p' ->
--         compileBody ss p'

--     addLd instr p = appendBody p [instr]
