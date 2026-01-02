{- 
-- EPITECH PROJECT, 2025
-- heap
-- File description:
-- hs
-}

module Heapelem (
        allocArray,
        allocObject,
        writeArray,
        writeField,
        readField,
        readArray
) where

import Data2
import qualified Data.Map as Map
import qualified Data.Vector as V

allocArray :: Int -> Value -> Heap -> (Handle, Heap)
allocArray size initVal heapp =
    let handle = V.length heapp
        newArray = HArray (V.replicate size initVal) 
        newHeap = V.snoc heapp newArray 
    in (handle, newHeap)


allocObject :: Map.Map String Value -> Heap -> (Handle, Heap)
allocObject fields heapp =
  let handle    = V.length heapp
      newObject = HObject fields
      newHeap   = V.snoc heapp newObject
  in (handle, newHeap)


readField :: Heap -> Handle -> String -> Either String Value
readField heapp hand field =
  case heapp V.!? hand of
    Just (HObject obj) ->
      case Map.lookup field obj of
        Just val -> Right val
        Nothing  -> Left ("Unknown field: " ++ field)

    Just _  -> Left "Handle is not an object"
    Nothing -> Left "Invalid handle"


readArray :: Heap -> Handle -> Int -> Either String Value
readArray heapp hand index =
  case heapp V.!? hand of
    Just (HArray arr) ->
      case arr V.!? index of
        Just val -> Right val
        Nothing  -> Left "Array index out of bounds"

    Just _  -> Left "Handle is not an array"
    Nothing -> Left "Invalid handle"


writeField :: Heap -> Handle -> String -> Value -> Either String Heap
writeField heapp hand field val =
  case heapp V.!? hand of
    Just (HObject obj) ->
      let newobj  = Map.insert field val obj
          newheap = heapp V.// [(hand, HObject newobj)]
      in Right (newheap)

    Just _  -> Left "Handle is not an object"
    Nothing -> Left "Invalid handle"
    

writeArray :: Heap -> Handle -> Int -> Value -> Either String Heap
writeArray heapp hand index val =
  case heapp V.!? hand of
    Just (HArray arr) ->
      case arr V.!? index of
        Nothing -> Left "Array index out of bounds"
        Just _  ->
          let newarr  = arr V.// [(index, val)]
              newheap = heapp V.// [(hand, HArray newarr)]
          in Right newheap

    Just _  -> Left "Handle is not an array"
    Nothing -> Left "Invalid handle"
