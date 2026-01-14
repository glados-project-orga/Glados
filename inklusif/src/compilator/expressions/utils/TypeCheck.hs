module TypeCheck
  ( isAssignable
  , expectType
  , expectIntLike
  , expectIntLike2
  ) where

import Ast (Type(..))

isAssignable :: Type -> Type -> Bool
isAssignable expected given = expected == given

expectType :: Type -> Type -> Either String ()
expectType expected given
  | isAssignable expected given = Right ()
  | otherwise = Left ("expected " ++ show expected ++ " but got " ++ show given)

isIntLike :: Type -> Bool
isIntLike IntType = True
isIntLike _ = False

expectIntLike :: Type -> Either String ()
expectIntLike t
  | isIntLike t = Right ()
  | otherwise = Left ("expected int but got " ++ show t)

expectIntLike2 :: Type -> Type -> Either String ()
expectIntLike2 a b
  | isIntLike a && isIntLike b = Right ()
  | otherwise =
      Left ("expected (int, int) but got (" ++ show a ++ ", " ++ show b ++ ")")
