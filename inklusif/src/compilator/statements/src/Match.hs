{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- match
-}

module Match (compileMatch) where

import CompilerTypes (CompilerData)
import CompilerTools (appendBody)
import Ast (MatchStmt(..), MatchCase(..), Pattern(..), Literal(..), Expr(..))
import Expr (compileExpr)
import Labels (generateLabel)
import EitherUtils (bindE)

compileMatch :: MatchStmt -> CompilerData -> Either String CompilerData
compileMatch (MatchStmt scrutinee cases) prog =
  bindE (compileExpr scrutinee prog) (\p1 ->
    let (lEnd, p2) = generateLabel p1
        (nonDef, mDef) = splitDefault cases
    in bindE (compileNonDefault lEnd nonDef p2) (\p3 ->
         bindE (compileDefaultIfAny lEnd mDef p3) (\p4 ->
           Right (emit (lEnd ++ ":") p4))))

compileDefaultIfAny :: String -> Maybe MatchCase -> CompilerData -> Either String CompilerData
compileDefaultIfAny _ Nothing p =
  Right (emit "pop" p)
compileDefaultIfAny lEnd (Just c) p =
  compileDefault lEnd c p

compileNonDefault :: String -> [MatchCase] -> CompilerData -> Either String CompilerData
compileNonDefault _ [] p = Right p
compileNonDefault lEnd (c:cs) p =
  bindE (compileOneCase lEnd c p) (\p' ->
    compileNonDefault lEnd cs p')

compileOneCase :: String -> MatchCase -> CompilerData -> Either String CompilerData
compileOneCase lEnd (MatchCase pat body) p =
  case pat of
    DefaultPattern -> Left "match: DefaultPattern must be last"
    LiteralPattern lit ->
      bindE (literalToComparableInt lit) (\litExpr ->
        let (lCase, p1) = generateLabel p
            (lNext, p2) = generateLabel p1
            p3 = emit "dup" p2
        in bindE (compileExpr litExpr p3) (\p4 ->
             let p5 = emit "isub" p4
                 p6 = emit ("ifeq " ++ lCase) p5
                 p7 = emit ("goto " ++ lNext) p6
                 p8 = emit (lCase ++ ":") p7
                 p9 = emit "pop" p8
             in bindE (compileExpr body p9) (\p10 ->
                  let p11 = emit ("goto " ++ lEnd) p10
                      p12 = emit (lNext ++ ":") p11
                  in Right p12)))

compileDefault :: String -> MatchCase -> CompilerData -> Either String CompilerData
compileDefault lEnd (MatchCase _ body) p =
  let p1 = emit "pop" p
  in bindE (compileExpr body p1) (\p2 ->
       Right (emit ("goto " ++ lEnd) p2))

splitDefault :: [MatchCase] -> ([MatchCase], Maybe MatchCase)
splitDefault [] = ([], Nothing)
splitDefault (c:cs) =
  case matchPattern c of
    DefaultPattern -> (cs, Just c)
    _ ->
      let (xs, d) = splitDefault cs
      in (c:xs, d)

emit :: String -> CompilerData -> CompilerData
emit instr prog = appendBody prog [instr]

literalToComparableInt :: Literal -> Either String Expr
literalToComparableInt (IntLit n)  = Right (LitExpr (IntLit n))
literalToComparableInt (BoolLit b) = Right (LitExpr (IntLit (if b then 1 else 0)))
literalToComparableInt (CharLit c) = Right (LitExpr (IntLit (fromEnum c)))
literalToComparableInt _ =
  Left "match: unsupported pattern literal type (only Int/Bool/Char supported)"
