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
