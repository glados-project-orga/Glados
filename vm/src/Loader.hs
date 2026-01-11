{- 
-- EPITECH PROJECT, 2025
-- mirror-repo
-- File description:
-- Loader.hs
-}

module Loader (
    loadBytecode,
    parseInstr,
    parseInstrs,
    parseFunction,
    parseFunctions
) where

import Data
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
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Applicative ((<|>))

loadBytecode :: String -> Either String (Map.Map String Function)
loadBytecode content =
    case runParser parseFunctions content of
        Left err -> Left err
        Right (funcs, _) -> Right (Map.fromList (map (\f -> (funcName f, f)) funcs))

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
         <|> parseInvokeStatic
         <|> parseReturn
         <|> parseArray
         <|> parseObject

parseKeyword :: String -> Parser ()
parseKeyword [] = pure ()
parseKeyword (c:cs) = parseChar c *> parseKeyword cs

parseConstInt :: Parser Instr
parseConstInt = parseKeyword "iconst" *> parseArgSep *> ((IStck_1 . IConstInt) <$> parseInt)

parseLdc :: Parser Instr
parseLdc = parseKeyword "ldc" *> parseSpaces *> (ILdc <$> parseInt)

parseLoadInt :: Parser Instr
parseLoadInt = parseKeyword "iload" *> parseArgSep *> ((IStck_1 . ILoadInt) <$> parseInt)

parseStoreInt :: Parser Instr
parseStoreInt = parseKeyword "istore" *> parseArgSep *> ((IStck_1 . IStoreInt) <$> parseInt)

parseArithmetic :: Parser Instr
parseArithmetic = (parseKeyword "iadd" *> pure (IOpInt IAddInt))
              <|> (parseKeyword "isub" *> pure (IOpInt ISubInt))
              <|> (parseKeyword "imul" *> pure (IOpInt IMulInt))
              <|> (parseKeyword "idiv" *> pure (IOpInt IDivInt))
              <|> (parseKeyword "irem" *> pure (IOpInt IRemInt))
              <|> (parseKeyword "ineg" *> pure (IOpInt INegInt))
              <|> (parseKeyword "iand" *> pure (IOpInt IAndInt))
              <|> (parseKeyword "ior" *>  pure (IOpInt IOrInt))
              <|> (parseKeyword "ixor" *> pure (IOpInt IXorInt))
              <|> (parseKeyword "ishl" *> pure (IOpInt IShlInt))
              <|> (parseKeyword "ishr" *> pure (IOpInt IShrInt))

parseStack :: Parser Instr
parseStack = (parseKeyword "dup2_x2" *> pure (IStck IDup2X2))
         <|> (parseKeyword "dup2_x1" *> pure (IStck IDup2X1))
         <|> (parseKeyword "dup_x2" *> pure (IStck IDupX2))
         <|> (parseKeyword "dup_x1" *> pure (IStck IDupX1))
         <|> (parseKeyword "dup2" *> pure (IStck IDup2))
         <|> (parseKeyword "dup" *> pure (IStck IDup))
         <|> (parseKeyword "pop2" *> pure (IStck IPop2))
         <|> (parseKeyword "pop" *> pure (IStck IPop))
         <|> (parseKeyword "swap" *> pure (IStck ISwap))
         <|> (parseKeyword "nop" *> pure (IStck INop))

parseControlFlow :: Parser Instr
parseControlFlow = parseIfICmpGt
               <|> parseIfICmpLt
               <|> parseIfEq
               <|> parseIfNe
               <|> parseIfGt
               <|> parseIfGe
               <|> parseIfLt
               <|> parseIfLe
               <|> parseGoto

parseGoto :: Parser Instr
parseGoto = parseKeyword "goto" *> parseSpaces *> (IGoto <$> parseInt)

parseIfEq :: Parser Instr
parseIfEq = parseKeyword "ifeq" *> parseSpaces *> (IIfEq <$> parseInt)

parseIfNe :: Parser Instr
parseIfNe = parseKeyword "ifne" *> parseSpaces *> (IIfNe <$> parseInt)

parseIfGt :: Parser Instr
parseIfGt = parseKeyword "ifgt" *> parseSpaces *> (IIfGt <$> parseInt)

parseIfGe :: Parser Instr
parseIfGe = parseKeyword "ifge" *> parseSpaces *> (IIfGe <$> parseInt)

parseIfLt :: Parser Instr
parseIfLt = parseKeyword "iflt" *> parseSpaces *> (IIfLt <$> parseInt)

parseIfLe :: Parser Instr
parseIfLe = parseKeyword "ifle" *> parseSpaces *> (IIfLe <$> parseInt)

parseIfICmpGt :: Parser Instr
parseIfICmpGt = parseKeyword "if_icmpgt" *> parseSpaces *> (IIfICmpGt <$> parseInt)

parseIfICmpLt :: Parser Instr
parseIfICmpLt = parseKeyword "if_icmplt" *> parseSpaces *> (IIfICmpLt <$> parseInt)

parseInvokeStatic :: Parser Instr
parseInvokeStatic = parseKeyword "invokestatic" *> (IInvokeStatic <$> parseString)

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

parseFunctions :: Parser [Function]
parseFunctions = parseMany (betweenSpaces parseFunction)

parseFunction :: Parser Function
parseFunction = 
    parseKeyword "fun" *>
    (Function <$> parseString 
              <*> (parseChar '{' *> parseSpaces *> 
                   (V.fromList <$> parseMany (betweenSpaces parseInstr)) 
                   <* parseSpaces <* parseChar '}'))
