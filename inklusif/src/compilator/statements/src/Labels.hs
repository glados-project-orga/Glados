{-
-- EPITECH PROJECT, 2026
-- glados-repo
-- File description:
-- Labels
-}

module Labels (
        LabelTable, 
        findLabels,
        correspondLabels,
        removeLabel,
        parseLabel,
        resolveLabels)
where

import qualified Data.Map as M
import CompilerTypes (CompilerData)

type LabelTable = M.Map String Int

parseLabel :: String -> Maybe String
parseLabel line =
  case span (/= ':') line of
    (('L':rest), ":") -> Just ('L':rest)
    _                 -> Nothing


findLabels :: [String] -> LabelTable
findLabels line = go 0 M.empty line
  where
    go _ table [] = table
    go n table (first_line:rest) =
      case parseLabel first_line of
        Just lab -> go n (M.insert lab n table) rest
        Nothing  -> go (n + 1) table rest


correspondLabels :: LabelTable -> [String] -> [String]
correspondLabels _ [] = []
correspondLabels table (l:ls) =
    (correspondLine table l : correspondLabels table ls)


correspondLine :: LabelTable -> String -> String
correspondLine table line =
  case words line of
    ["ifeq", lab] ->
      case M.lookup lab table of
        Just n  -> "ifeq " ++ show n
        Nothing -> error ("Label inconnu: " ++ lab)

    ["goto", lab] ->
      case M.lookup lab table of
        Just n  -> "goto " ++ show n
        Nothing -> error ("Label inconnu: " ++ lab)
  
    _ -> line


removeLabel :: String -> Bool
removeLabel line =
  case parseLabel line of
    Just _  -> False
    Nothing -> True


removeLabels :: [String] -> [String]
removeLabels = filter removeLabel


resolveLabels :: CompilerData -> CompilerData
resolveLabels (cp, defs, bytecode, symtab) =
    let table   = findLabels bytecode
        bc1     = correspondLabels table bytecode
        bcFinal = removeLabelLines bc1
    in (cp, defs, bcFinal, symtab)
