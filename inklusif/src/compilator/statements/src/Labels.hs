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
        generateLabel,
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

    [op, lab] 
      | op `elem` 
          [ "goto" , "ifeq",  "ifne", "iflt",
            "ifle", "ifgt", "ifge" , "if_icmpeq",
            "if_icmpne", "if_icmplt" , "if_icmple",
            "if_icmpgt", "if_icmpge"
          ] ->
              case M.lookup lab table of 
                Just n -> op ++ " " ++ show n
                Nothing -> error ("Label inconnu: " ++ lab)
    _ -> line


resolveLabels :: CompilerData -> CompilerData
resolveLabels (cp, defs, bytecode, symtab) =
    let table   = findLabels bytecode
        bc1     = correspondLabels table bytecode
        bcFinal = removeLabels bc1
    in (cp, defs, bcFinal, symtab)


generateLabel :: CompilerData -> (String, CompilerData)
generateLabel (cp, (heap, funcs, classes, enums, typedefs, counter), bc, sym) = 
    let label = "L" ++ show counter
        newDefines = (heap, funcs, classes, enums, typedefs, counter + 1)
    in (label, (cp, newDefines, bc, sym))


removeLabel :: String -> Bool
removeLabel line =
  case parseLabel line of
    Just _  -> False
    Nothing -> True


removeLabels :: [String] -> [String]
removeLabels = filter removeLabel