{- 
-- EPITECH PROJECT, 2025
-- mirror-repo
-- File description:
-- Loader.hs
-}

module Loader (
    loadBytecode,
    parseInstr,
    parseInstrs
) where

import Data (Instr(..))
import Parser (
    Parser(..),
    runParser,
    parseChar,
    parseSpaces,
    betweenSpaces,
    parseMany,
    parseInt,
    parseArgSep,
    parseString
  )
import Control.Applicative ((<|>))

loadBytecode :: String -> Either String [Instr]
loadBytecode content =
    case runParser parseInstrs content of
        Left err -> Left err
        Right (instrs, _) -> Right instrs

parseInstrs :: Parser [Instr]
parseInstrs = parseMany (betweenSpaces parseInstr)

parseInstr :: Parser Instr
parseInstr = parseConstInt
         <|> parseLdc
         <|> parseLoadInt
         <|> parseStoreInt
         <|> parseArithmetic
         <|> parseStack
         <|> parseControlFlow
         <|> parseInvoke
         <|> parseReturn
         <|> parseArray
         <|> parseObject

parseKeyword :: String -> Parser ()
parseKeyword [] = pure ()
parseKeyword (c:cs) = parseChar c *> parseKeyword cs

parseConstInt :: Parser Instr
parseConstInt = parseKeyword "iconst" *> parseArgSep *> (IConstInt <$> parseInt)

parseLdc :: Parser Instr
parseLdc = parseKeyword "ldc" *> parseSpaces *> (ILdc <$> parseInt)

parseLoadInt :: Parser Instr
parseLoadInt = parseKeyword "iload" *> parseArgSep *> (ILoadInt <$> parseInt)

parseStoreInt :: Parser Instr
parseStoreInt = parseKeyword "istore" *> parseArgSep *> (IStoreInt <$> parseInt)

parseArithmetic :: Parser Instr
parseArithmetic = (parseKeyword "iadd" *> pure IAddInt)
              <|> (parseKeyword "isub" *> pure ISubInt)
              <|> (parseKeyword "imul" *> pure IMulInt)
              <|> (parseKeyword "idiv" *> pure IDivInt)
              <|> (parseKeyword "irem" *> pure IRemInt)
              <|> (parseKeyword "ineg" *> pure INegInt)
              <|> (parseKeyword "iand" *> pure IAndInt)
              <|> (parseKeyword "ior" *> pure IOrInt)
              <|> (parseKeyword "ixor" *> pure IXorInt)
              <|> (parseKeyword "ishl" *> pure IShlInt)
              <|> (parseKeyword "ishr" *> pure IShrInt)
              <|> parseIncInt

parseIncInt :: Parser Instr
parseIncInt = parseKeyword "iinc" *> parseSpaces *>
              (IIncInt <$> parseInt <*> (parseSpaces *> parseInt))

parseStack :: Parser Instr
parseStack = (parseKeyword "dup2_x2" *> pure IDup2X2)
         <|> (parseKeyword "dup2_x1" *> pure IDup2X1)
         <|> (parseKeyword "dup_x2" *> pure IDupX2)
         <|> (parseKeyword "dup_x1" *> pure IDupX1)
         <|> (parseKeyword "dup2" *> pure IDup2)
         <|> (parseKeyword "dup" *> pure IDup)
         <|> (parseKeyword "pop2" *> pure IPop2)
         <|> (parseKeyword "pop" *> pure IPop)
         <|> (parseKeyword "swap" *> pure ISwap)
         <|> (parseKeyword "nop" *> pure INop)

parseControlFlow :: Parser Instr
parseControlFlow = parseIfICmpGt
               <|> parseIfICmpLt
               <|> parseIfEq
               <|> parseIfGt
               <|> parseIfLt
               <|> parseGoto

parseGoto :: Parser Instr
parseGoto = parseKeyword "goto" *> parseSpaces *> (IGoto <$> parseInt)

parseIfEq :: Parser Instr
parseIfEq = parseKeyword "ifeq" *> parseSpaces *> (IIfEq <$> parseInt)

parseIfGt :: Parser Instr
parseIfGt = parseKeyword "ifgt" *> parseSpaces *> (IIfGt <$> parseInt)

parseIfLt :: Parser Instr
parseIfLt = parseKeyword "iflt" *> parseSpaces *> (IIfLt <$> parseInt)

parseIfICmpGt :: Parser Instr
parseIfICmpGt = parseKeyword "if_icmpgt" *> parseSpaces *> (IIfICmpGt <$> parseInt)

parseIfICmpLt :: Parser Instr
parseIfICmpLt = parseKeyword "if_icmplt" *> parseSpaces *> (IIfICmpLt <$> parseInt)

parseInvoke :: Parser Instr
parseInvoke = parseInvokeStatic
          <|> parseInvokeVirtual
          <|> parseInvokeSpecial

parseInvokeStatic :: Parser Instr
parseInvokeStatic = parseKeyword "invokestatic" *> (IInvokeStatic <$> parseString)

parseInvokeVirtual :: Parser Instr
parseInvokeVirtual = parseKeyword "invokevirtual" *> (IInvokeVirtual <$> parseString)

parseInvokeSpecial :: Parser Instr
parseInvokeSpecial = parseKeyword "invokespecial" *> (IInvokeSpecial <$> parseString)

parseReturn :: Parser Instr
parseReturn = (parseKeyword "ireturn" *> pure IReturnInt)
          <|> (parseKeyword "return" *> pure IReturn)

parseObject :: Parser Instr
parseObject = parseNew
          <|> parseGetField
          <|> parsePutField

parseNew :: Parser Instr
parseNew = parseKeyword "new" *> (INew <$> parseString)

parseGetField :: Parser Instr
parseGetField = parseKeyword "getfield" *> (IGetField <$> parseString)

parsePutField :: Parser Instr
parsePutField = parseKeyword "putfield" *> (IPutField <$> parseString)

parseArray :: Parser Instr
parseArray = (parseKeyword "newarray" *> pure INewArray)
         <|> (parseKeyword "iaload" *> pure IALoad)
         <|> (parseKeyword "iastore" *> pure IAStore)
         <|> (parseKeyword "arraylength" *> pure IArrayLength)
