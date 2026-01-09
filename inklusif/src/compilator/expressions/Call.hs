{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

module Call (emitCall, compileCallInstr) where

import CompilerTypes (ProgramLayer)
import CompilerTools (appendBody)

emitCall :: String -> [String]
emitCall fname = ["invokestatic " ++ fname]

compileCallInstr :: String -> ProgramLayer -> ProgramLayer
compileCallInstr fname (bin, st) = (appendBody bin (emitCall fname), st)
