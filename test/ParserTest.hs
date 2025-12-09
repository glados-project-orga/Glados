{-
-- EPITECH PROJECT, 2025
-- parserTest
-- File description:
-- unit tests for parser
-}

module ParserTest (
    testParser
) where

import Test.HUnit
import Parser (
    Parser(..),
    parseChar,
    sepBy,
    parseAnyChar,
    parseSpaces,
    betweenSpaces,
    parseOr,
    parseSome,
    parseMany,
    parseUInt,
    parseInt
    )

testParseChar :: Test
testParseChar = TestList [
    TestCase (assertEqual "parseChar with valid string" (Right ('a', "bc")) (runParser (parseChar 'a') "abc")),
    TestCase (assertEqual "parseChar with string not containing char" (Left "Character 'a' not found") (runParser (parseChar 'a') "bc")),
    TestCase (assertEqual "parseChar with empty string" (Left "Expected 'a' but reached end of input") (runParser (parseChar 'a') ""))
    ]

testSepBy :: Test
testSepBy = TestList [
    TestCase (assertEqual "sepBy parseChar ',' with valid string'" (Right ([1,2,3], "")) (runParser (sepBy ( parseInt) (parseChar ',')) "1,2,3"))
    ]

testParseAnyChar :: Test
testParseAnyChar = TestList [
    TestCase (assertEqual "parseAnyChar with valid string" (Right ('a', "bc")) (runParser (parseAnyChar "a") "abc")),
    TestCase (assertEqual "parseAnyChar with invalid string" (Left "Character 'b' not found in the entier string") (runParser (parseAnyChar "az") "bc")),
    TestCase (assertEqual "parseAnyChar on empty string" (Left "Reached end of input") (runParser (parseAnyChar "a") ""))
    ]

testParseSpaces :: Test
testParseSpaces = TestList [
    TestCase (assertEqual "parseSpaces with spaces" (Right ((), "abc")) (runParser parseSpaces "   abc")),
    TestCase (assertEqual "parseSpaces without spaces" (Right ((), "abc")) (runParser parseSpaces "abc")),
    TestCase (assertEqual "parseSpaces with only spaces" (Right ((), "")) (runParser parseSpaces "     ")),
    TestCase (assertEqual "parseSpaces with empty string" (Right ((), "")) (runParser parseSpaces ""))
    ]

testBetweenSpaces :: Test
testBetweenSpaces = TestList [
    TestCase (assertEqual "betweenSpaces parseChar with spaces around" (Right ('a', "bc")) (runParser (betweenSpaces (parseChar 'a')) " a bc")),
    TestCase (assertEqual "betweenSpaces parseChar without any spaces" (Right ('a', "bc")) (runParser (betweenSpaces (parseChar 'a')) "abc"))
    ]

testParseOr :: Test
testParseOr = TestList [
    TestCase (assertEqual "parseOr with first parser succeeding" (Right ('a', "bc")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abc")),
    TestCase (assertEqual "parseOr with second parser succeeding" (Right ('b', "ac")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "bac")),
    TestCase (assertEqual "parseOr with both parsers failing" (Left "Character 'b' not found") (runParser (parseOr (parseChar 'a') (parseChar 'b')) "cde"))
    ]

testParseUInt :: Test
testParseUInt = TestList [
    TestCase (assertEqual "parseUInt with valid number" (Right (123, "abc")) (runParser parseUInt "123abc")),
    TestCase (assertEqual "parseUInt with invalid number" (Left "Character 'a' not found in the entier string") (runParser parseUInt "abc")),
    TestCase (assertEqual "parseUint with negative number" (Left "Character '-' not found in the entier string") (runParser parseUInt "-123abc")),
    TestCase (assertEqual "parseUInt on empty string" (Left "Reached end of input") (runParser parseUInt ""))
    ]


testParser :: Test
testParser = TestList [
    testParseChar,
    testSepBy,
    testParseAnyChar,
    testParseSpaces,
    testBetweenSpaces,
    testParseOr,
    testParseUInt
    ]