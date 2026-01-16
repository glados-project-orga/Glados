module Lambda (compileLambda) where

import CompilerTypes (CompilerData)
import Ast (Parameter, Statement)

compileLambda :: ([Parameter], [Statement]) -> CompilerData -> Either String CompilerData
compileLambda _ prog = Right prog