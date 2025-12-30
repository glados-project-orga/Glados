{- 
-- EPITECH PROJECT, 2025
-- heap
-- File description:
-- hs
-}

import Data2

import qualified Data.Map as Map


allocObjet :: Heap -> Map.Map String Value -> (Heap, Handle)
allocObjet heap object =
    let val = if Map.null heap then 0 else (fst(Map.findMax(heap)) + 1)
        newheap = Map.insert val (HObject object) heap
    in (newheap, val)


allocArray :: Heap -> [Value] -> (Heap, Handle)
allocArray heap values =
    let val = if Map.null heap then 0 else (fst(Map.findMax(heap)) + 1)
        newheap = Map.insert val (HArray values) heap
    in  (newheap, val)


readField :: Heap -> Handle -> String -> Either String Value
readField heap hand stt =
    case Map.lookup hand heap of
        Just (HObject object) ->
            case Map.lookup stt object of
                Just val -> Right val
                _ -> Left ("Unknown field: " ++ stt)
        _ -> Left "Handle is not an object"


readArray :: Heap -> Handle -> Int -> Either String Value
readArray heap hand index =
    case Map.lookup hand heap of
        Just (HArray tab) ->
            if  index < 0 || index >= length tab
                then Left  "Array index out of bounds"
            else Right (tab !! index)
        _   -> Left "Handle is not an array"
