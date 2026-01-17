{- 
-- EPITECH PROJECT, 2025
-- mirror-repo
-- File description:
-- Loader.hs
-}

module Loader (
    loadBytecode
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
    parseLong,
    parseFloat,
    parseDouble,
    parseSingleChar,
    parseArgSep,
    parseString,
    parseQuotedString,
    parseKeyword,
    parseBool,
    sepBy
  )
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Applicative ((<|>))

loadBytecode :: String -> Either String (V.Vector Value, Map.Map String Function)
loadBytecode content =
    case runParser parseBytecodeFile content of
        Left err -> Left err
        Right ((pool, funcs), _) -> 
            Right (pool, Map.fromList (map (\f -> (funcName f, f)) funcs))

parseInstr :: Parser Instr
parseInstr = parseConsts
         <|> parseLdc
         <|> parseLoadStore
         <|> parseArithmetic
         <|> parseStack
         <|> parseControlFlow
         <|> parseInvokeStatic
         <|> parseInvokeWrite
         <|> parseReturn
         <|> parseArray
         <|> parseObject
         <|> parseConversion
         <|> parseComparison

parseConsts :: Parser Instr
parseConsts = parseConstInt
          <|> parseConstLong
          <|> parseConstFloat
          <|> parseConstDouble
          <|> parseConstChar
          <|> parseConstString

parseConstInt :: Parser Instr
parseConstInt = parseKeyword "iconst" *> parseArgSep *> ((IStck_1 . IConstInt) <$> parseInt)

parseConstLong :: Parser Instr
parseConstLong = parseKeyword "lconst" *> parseArgSep *> ((IStck_1 . IConstLong) <$> parseLong)

parseConstFloat :: Parser Instr
parseConstFloat = parseKeyword "fconst" *> parseArgSep *> ((IStck_1 . IConstFloat) <$> parseFloat)

parseConstDouble :: Parser Instr
parseConstDouble = parseKeyword "dconst" *> parseArgSep *> ((IStck_1 . IConstDouble) <$> parseDouble)

parseConstChar :: Parser Instr
parseConstChar = parseKeyword "cconst" *> parseArgSep *> ((IStck_1 . IConstChar) <$> parseSingleChar)

parseConstString :: Parser Instr
parseConstString = parseKeyword "sconst" *> parseArgSep *> ((IStck_1 . IConstString) <$> parseQuotedString)

parseLdc :: Parser Instr
parseLdc = parseKeyword "ldc" *> parseSpaces *> (ILdc <$> parseInt)

parseLoadStore :: Parser Instr
parseLoadStore = parseLoadInt
             <|> parseStoreInt
             <|> parseLoadFloat
             <|> parseStoreFloat
             <|> parseLoadLong
             <|> parseStoreLong
             <|> parseLoadDouble
             <|> parseStoreDouble
             <|> parseLoadChar
             <|> parseStoreChar
             <|> parseALoad
             <|> parseAStore
             <|> parseIinc

parseLoadInt :: Parser Instr
parseLoadInt = parseKeyword "iload" *> parseArgSep *> ((IStck_1 . ILoadInt) <$> parseInt)

parseStoreInt :: Parser Instr
parseStoreInt = parseKeyword "istore" *> parseArgSep *> ((IStck_1 . IStoreInt) <$> parseInt)

parseLoadFloat :: Parser Instr
parseLoadFloat = parseKeyword "fload" *> parseArgSep *> ((IStck_1 . ILoadFloat) <$> parseInt)

parseStoreFloat :: Parser Instr
parseStoreFloat = parseKeyword "fstore" *> parseArgSep *> ((IStck_1 . IStoreFloat) <$> parseInt)

parseLoadLong :: Parser Instr
parseLoadLong = parseKeyword "lload" *> parseArgSep *> ((IStck_1 . ILoadLong) <$> parseInt)

parseStoreLong :: Parser Instr
parseStoreLong = parseKeyword "lstore" *> parseArgSep *> ((IStck_1 . IStoreLong) <$> parseInt)

parseLoadDouble :: Parser Instr
parseLoadDouble = parseKeyword "dload" *> parseArgSep *> ((IStck_1 . ILoadDouble) <$> parseInt)

parseStoreDouble :: Parser Instr
parseStoreDouble = parseKeyword "dstore" *> parseArgSep *> ((IStck_1 . IStoreDouble) <$> parseInt)

parseLoadChar :: Parser Instr
parseLoadChar = parseKeyword "cload" *> parseArgSep *> ((IStck_1 . ILoadChar) <$> parseInt)

parseStoreChar :: Parser Instr
parseStoreChar = parseKeyword "cstore" *> parseArgSep *> ((IStck_1 . IStoreChar) <$> parseInt)

parseALoad :: Parser Instr
parseALoad = parseKeyword "aload" *> parseArgSep *> ((IStck_1 . ALoad) <$> parseInt)

parseAStore :: Parser Instr
parseAStore = parseKeyword "astore" *> parseArgSep *> ((IStck_1 . AStore) <$> parseInt)

parseIinc :: Parser Instr
parseIinc = (\idx inc -> IIinc idx inc)
        <$> (parseKeyword "iinc" *> parseArgSep *> parseInt)
        <*> (parseArgSep *> parseInt)

parseArithmetic :: Parser Instr
parseArithmetic = parseIntArithmetic
              <|> parseFloatArithmetic
              <|> parseDoubleArithmetic
              <|> parseLongArithmetic

parseIntArithmetic :: Parser Instr
parseIntArithmetic = (parseKeyword "iadd" *> pure (IOpInt IAddInt))
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

parseFloatArithmetic :: Parser Instr
parseFloatArithmetic = (parseKeyword "fadd" *> pure (IOpFloat FAddFloat))
              <|> (parseKeyword "fsub" *> pure (IOpFloat FSubFloat))
              <|> (parseKeyword "fmul" *> pure (IOpFloat FMulFloat))
              <|> (parseKeyword "fdiv" *> pure (IOpFloat FDivFloat))
              <|> (parseKeyword "frem" *> pure (IOpFloat FRemFloat))
              <|> (parseKeyword "fneg" *> pure (IOpFloat FNegFloat))

parseDoubleArithmetic :: Parser Instr
parseDoubleArithmetic = (parseKeyword "dadd" *> pure (IOpDouble DAddDouble))
              <|> (parseKeyword "dsub" *> pure (IOpDouble DSubDouble))
              <|> (parseKeyword "dmul" *> pure (IOpDouble DMulDouble))
              <|> (parseKeyword "ddiv" *> pure (IOpDouble DDivDouble))
              <|> (parseKeyword "drem" *> pure (IOpDouble DRemDouble))
              <|> (parseKeyword "dneg" *> pure (IOpDouble DNegDouble))

parseLongArithmetic :: Parser Instr
parseLongArithmetic = (parseKeyword "ladd" *> pure (IOpLong LAddLong))
              <|> (parseKeyword "lsub" *> pure (IOpLong LSubLong))
              <|> (parseKeyword "lmul" *> pure (IOpLong LMulLong))
              <|> (parseKeyword "ldiv" *> pure (IOpLong LDivLong))
              <|> (parseKeyword "lrem" *> pure (IOpLong LRemLong))
              <|> (parseKeyword "lneg" *> pure (IOpLong LNegLong))
              <|> (parseKeyword "land" *> pure (IOpLong LAndLong))
              <|> (parseKeyword "lor" *>  pure (IOpLong LOrLong))
              <|> (parseKeyword "lxor" *> pure (IOpLong LXorLong))
              <|> (parseKeyword "lshl" *> pure (IOpLong LShlLong))
              <|> (parseKeyword "lshr" *> pure (IOpLong LShrLong))

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
               <|> parseIfICmpEq
               <|> parseIfICmpNe
               <|> parseIfICmpGe
               <|> parseIfICmpLe
               <|> parseIfACmpEq
               <|> parseIfACmpNe
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

parseIfICmpEq :: Parser Instr
parseIfICmpEq = parseKeyword "if_icmpeq" *> parseSpaces *> (IIfICmpEq <$> parseInt)

parseIfICmpNe :: Parser Instr
parseIfICmpNe = parseKeyword "if_icmpne" *> parseSpaces *> (IIfICmpNe <$> parseInt)

parseIfICmpGe :: Parser Instr
parseIfICmpGe = parseKeyword "if_icmpge" *> parseSpaces *> (IIfICmpGe <$> parseInt)

parseIfICmpLe :: Parser Instr
parseIfICmpLe = parseKeyword "if_icmple" *> parseSpaces *> (IIfICmpLe <$> parseInt)

parseIfACmpEq :: Parser Instr
parseIfACmpEq = parseKeyword "if_acmpeq" *> parseSpaces *> (IIfACmpEq <$> parseInt)

parseIfACmpNe :: Parser Instr
parseIfACmpNe = parseKeyword "if_acmpne" *> parseSpaces *> (IIfACmpNe <$> parseInt)

parseInvokeStatic :: Parser Instr
parseInvokeStatic = parseKeyword "invokestatic" *> (IInvokeStatic <$> parseString)

parseInvokeWrite :: Parser Instr
parseInvokeWrite = parseKeyword "invoke_write" *> parseArgSep *> (IInvokeWrite <$> parseInt)

parseReturn :: Parser Instr
parseReturn = (parseKeyword "ireturn" *> pure IReturnInt)
          <|> (parseKeyword "dreturn" *> pure IReturnDouble)
          <|> (parseKeyword "freturn" *> pure IReturnFloat)
          <|> (parseKeyword "lreturn" *> pure IReturnLong)
          <|> (parseKeyword "areturn" *> pure IReturnA)
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

parseConversion :: Parser Instr
parseConversion = (parseKeyword "i2c" *> pure (IConv I2c))
              <|> (parseKeyword "i2l" *> pure (IConv I2l))
              <|> (parseKeyword "i2f" *> pure (IConv I2f))
              <|> (parseKeyword "i2d" *> pure (IConv I2d))
              <|> (parseKeyword "l2i" *> pure (IConv L2i))
              <|> (parseKeyword "l2f" *> pure (IConv L2f))
              <|> (parseKeyword "l2d" *> pure (IConv L2d))
              <|> (parseKeyword "f2i" *> pure (IConv F2i))
              <|> (parseKeyword "f2l" *> pure (IConv F2l))
              <|> (parseKeyword "f2d" *> pure (IConv F2d))
              <|> (parseKeyword "d2i" *> pure (IConv D2i))
              <|> (parseKeyword "d2l" *> pure (IConv D2l))
              <|> (parseKeyword "d2f" *> pure (IConv D2f))
              <|> (parseKeyword "c2i" *> pure (IConv C2i))

parseComparison :: Parser Instr
parseComparison = (parseKeyword "lcmp" *> pure ILcmp)
              <|> (parseKeyword "fcmpl" *> pure IFcmpl)
              <|> (parseKeyword "fcmpg" *> pure IFcmpg)
              <|> (parseKeyword "dcmpl" *> pure IDcmpl)
              <|> (parseKeyword "dcmpg" *> pure IDcmpg)

parseFunctions :: Parser [Function]
parseFunctions = parseMany (betweenSpaces parseFunction)

parseFunction :: Parser Function
parseFunction = 
    parseKeyword "fun" *>
    (Function <$> parseString 
              <*> (parseChar '{' *> parseSpaces *> 
                   (V.fromList <$> parseMany (betweenSpaces parseInstr)) 
                   <* parseSpaces <* parseChar '}'))

parseBytecodeFile :: Parser (V.Vector Value, [Function])
parseBytecodeFile = 
    (,) <$> (parseSpaces *> parseHeader)
        <*> parseFunctions

parseHeader :: Parser (V.Vector Value)
parseHeader = (parseKeyword "header" *> parseSpaces *> 
               parseChar '{' *> parseSpaces *>
               (V.fromList <$> (parseValue `sepBy` parseSemicolon))
               <* parseSpaces <* parseChar '}' <* parseSpaces)
          <|> pure V.empty

parseSemicolon :: Parser ()
parseSemicolon = parseSpaces *> parseChar ';' *> parseSpaces

parseValue :: Parser Value
parseValue = parseSpaces *> parseValue' <* parseSpaces
  where
    parseValue' = (VBool <$> parseBool)
              <|> (VChar <$> parseSingleChar)
              <|> (VString <$> parseQuotedString)
              <|> (VDouble <$> parseDouble)
              <|> (VFloat <$> parseFloat)
              <|> (VLong <$> parseLong)
              <|> (VInt <$> parseInt)
