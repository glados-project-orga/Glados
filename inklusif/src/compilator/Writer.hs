module Writer (writeBytecode) where

import CompilerTypes (CompilerData, ConstantPool, Bytecode)

writeBytecode :: CompilerData -> String
writeBytecode (cp, _defs, bc, _st) =
  writeHeader cp ++ writeBody bc

writeHeader :: ConstantPool -> String
writeHeader cp =
  "header {\n"
  ++ writeConstants cp
  ++ "}\n"

writeConstants :: ConstantPool -> String
writeConstants [] = ""
writeConstants (x:xs) =
  x ++ ";\n" ++ writeConstants xs

writeBody :: Bytecode -> String
writeBody [] = ""
writeBody (x:xs) =
  x ++ "\n" ++ writeBody xs
