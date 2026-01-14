module Enum (compileEnum, getEnumValue) where
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import CompilerError (errPos)
import CompilerTools (appendDefines)
import Ast (EnumDecl(..), EnumField(..), Declaration(..))
import CompilerTypes (CompilerData)
import Data.List (find)

getFromEnum :: [EnumField] -> String -> Maybe Int
getFromEnum enumfields searched =
    find (\enumfield -> declName enumfield == searched) enumfields >>= declValue

getEnumValue :: [EnumDecl] -> String -> Maybe Int
getEnumValue enums searched =
    asum $ map (\enum -> getFromEnum (enumDecl enum) searched) enums

enumNameExists :: String -> [EnumDecl] -> Bool
enumNameExists searched enums =
    any (\enum -> enumName enum == searched) enums

enumFieldExists :: [EnumField] -> [EnumDecl] -> Bool
enumFieldExists fields enums =
    (any (\field -> getEnumValue enums (declName field) /= Nothing) fields)

fillEnumValues :: [EnumField] -> Int -> [EnumField]
fillEnumValues fields startVal =
    zipWith (\field val -> field {declValue = Just val}) fields [startVal..]

compileEnum :: EnumDecl -> CompilerData -> Either String CompilerData
compileEnum (EnumDecl pos name []) _ =
    Left ((errPos pos) ++ "Enum " ++ name ++ " must have at least one field.")
compileEnum enum@(EnumDecl pos name fields@(field:_)) prog@(_, (_, _, enums, _), _, _)
    | enumNameExists name enums =
        Left ((errPos pos) ++ "Enum " ++ name ++ " is already defined.")
    | enumFieldExists fields enums =
        Left ((errPos pos) ++ "Enum " ++ name ++ " has fields that are already defined.")
    | otherwise = Right $ appendDefines prog [(Enum enum {enumDecl = filledFields})]
        where filledFields = fillEnumValues fields startVal
              startVal = fromMaybe 0 (declValue field)