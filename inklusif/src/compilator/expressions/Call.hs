{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Call (emitCall, compileCallInstr) where

import CompilerTypes (CompilerData)
import CompilerTools (appendBody)

emitCall :: String -> [String]
emitCall fname = ["invokestatic " ++ fname]

compileCallInstr :: String -> CompilerData -> CompilerData
compileCallInstr fname prog = (appendBody prog (emitCall fname))
