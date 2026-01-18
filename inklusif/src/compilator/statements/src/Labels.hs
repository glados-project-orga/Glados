{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- Labels
-}

module Labels (
  LabelTable,
  parseLabel,
  generateLabel,
  resolveLabels
) where

import qualified Data.Map as M
import CompilerTypes (CompilerData)

type LabelTable = M.Map String Int

parseLabel :: String -> Maybe String
parseLabel line =
  case span (/= ':') line of
    (('L':rest), ":") -> Just ('L':rest)
    _                 -> Nothing

isDirective :: String -> Bool
isDirective line =
  case words line of
    ["header", "{"]     -> True
    ["}"]               -> True
    ["fun", _name, "{"] -> True
    _                   -> False

correspondLine :: LabelTable -> String -> String
correspondLine table line =
  case words line of
    [op, lab]
      | op `elem`
          [ "goto", "ifeq", "ifne", "iflt", "ifle", "ifgt", "ifge"
          , "if_icmpeq", "if_icmpne", "if_icmplt", "if_icmple"
          , "if_icmpgt", "if_icmpge"
          ] ->
          case M.lookup lab table of
            Just n  -> op ++ " " ++ show n
            Nothing -> error ("Unknown label: " ++ lab)
    _ -> line

resolveLabelTable :: [String] -> (LabelTable, [String])
resolveLabelTable = go 0 M.empty []
  where
    go _ table acc [] = (table, reverse acc)
    go pc table acc (line:rest)
      | isDirective line =
          go pc table (line:acc) rest
      | otherwise =
          case parseLabel line of
            Just lab -> go pc (M.insert lab pc table) acc rest
            Nothing  -> go (pc + 1) table (line:acc) rest

resolveLabels :: CompilerData -> CompilerData
resolveLabels (cp, defs, bytecode, symtab) =
  let (table, bcNoLab) = resolveLabelTable bytecode
      bcFinal          = map (correspondLine table) bcNoLab
  in (cp, defs, bcFinal, symtab)

generateLabel :: CompilerData -> (String, CompilerData)
generateLabel (cp, (heap, funcs, classes, enums, typedefs, counter), bc, sym) =
  let label = "L" ++ show counter
      newDefines = (heap, funcs, classes, enums, typedefs, counter + 1)
  in (label, (cp, newDefines, bc, sym))
