{-
-- EPITECH PROJECT, 2026
-- glados
-- File description:
-- expr
-}

-- helper pour eviter case of et (>>= et >>) apparents

module EitherUtils
  ( bindE
  , thenE
  ) where

bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left err) _ = Left err
bindE (Right x) f  = f x

thenE :: Either e a -> Either e b -> Either e b
thenE (Left err) _  = Left err
thenE (Right _) nxt = nxt
