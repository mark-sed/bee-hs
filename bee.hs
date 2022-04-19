{- |
Module     : bee.hs
Description: Bee language compiler
Author     : Marek Sedlacek
Email      : mr.mareksedlacek@gmail.com
Date       : April 2022
Copyright  : 2022 Marek Sedlacek. All rights reserved.

Bee language compiler written in haskell.
The compiler compiles down to Ebel language.
-}

module Main where

import System.Environment
import Data.Char

-- | Ebel instructions
-- | Show will be used to output them.
data Instruction = CONCAT Int
                 | DEL
                 | LOOP
                 | NOP
                 | SWAP Int
                 deriving Show

-- | Possible pass types
data PassType = WordsPass 
              | LinesPass 
              | ExpressionPass String
              deriving (Show, Eq)

-- | Pass IR
type Pass = (PassType, [Instruction])

-- | Ebel IR
type Ebel = [Pass]

-- | Parsing state to correctly parse strings
data SplitState = SSSTART 
                | SSSTRING 
                | SSCOMMENT
                deriving (Enum, Show, Eq)

-- | Delimiters, which are ignored by Bee
beeDelims :: [Char]
beeDelims = [' ', '\t', '\n']

-- | Symbols recognized by Bee
beeSymbols :: [Char]
beeSymbols = [':', ';', '?', '(', ')', '{', '}', '@', '?']

-- | Function to print error 
fatalError :: String -> a
fatalError msg = error ("ERROR: " ++ msg ++ ".")

-- | Splits string into lexemes
splitDelim :: String -> (String, SplitState) -> (SplitState, [String])
splitDelim (x:xs) (lst, s)
    | s == SSCOMMENT && x == '\n' = splitDelim xs ("", SSSTART)
    | s == SSCOMMENT = s7
    | s == SSSTRING && x == '"' = (fst s3, (lst++[x]) : snd s3)
    | s == SSSTRING && x == '\n' = fatalError "Missing closing string quote"
    | s == SSSTRING = splitDelim xs (lst++[x], SSSTRING)
    | x == '"' && null lst = s5
    | x == '"' = (fst s5, lst : snd s5)
    | x == '#' && null lst = (fst s7, snd s7)
    | x == '#' = (fst s7, lst : snd s7)
    | x `elem` beeSymbols && null lst = (fst s1, [x] : snd s1)
    | x `elem` beeSymbols = (fst s1, lst : [x] : snd s1)
    | x `elem` beeDelims && null lst = (fst s1, snd s1)
    | x `elem` beeDelims = (fst s1, lst : snd s1)
    | otherwise = splitDelim xs (lst++[x], s)
    where
        s1 = splitDelim xs ("", s)
        s3 = splitDelim xs ("", SSSTART)
        s5 = splitDelim xs ("\"", SSSTRING)
        s7 = splitDelim xs ("", SSCOMMENT)
splitDelim [] ([], s) = (s, [])
splitDelim [] (lst, s) = (s, [lst])

-- | Parses one pass in the code
parsePass :: [String] -> ([Instruction], [String])
parsePass [] = ([], [])
parsePass t@("{":_) = ([], t)
parsePass t@(p:":":_) 
    -- New pass
    | p == "W" || p == "L" || p == "Words" || p == "Lines" = ([], t)
parsePass (i:";":ts)
    -- 0 argument instructions
    | iu == "DEL"  = (DEL : (fst p), snd p)
    | iu == "LOOP" || iu == "}" = (LOOP : (fst p), snd p)
    | iu == "NOP"  = (NOP  : (fst p), snd p)
    where p = parsePass ts
          iu = (map toUpper) i
parsePass (i:v:";":ts)
    -- 1 argument instructions
    | iu == "CONCAT" = (CONCAT (read v::Int) : (fst p), snd p)
    | iu == "SWAP"   = (SWAP   (read v::Int) : (fst p), snd p)
    -- 0 argument instructions with multiplier
    | mu < 1 = fatalError "Instruction multiplier has to be positive integer"
    | vu == "DEL"  = ((take mu $ repeat DEL) ++ (fst p), snd p)
    | vu == "LOOP" = ((take mu $ repeat LOOP) ++ (fst p), snd p)
    | vu == "NOP"  = ((take mu $ repeat NOP)  ++ (fst p), snd p)
    | otherwise = fatalError ("Unknwon instruction '"++i++"'")
    where p = parsePass ts
          iu = map toUpper i
          vu = map toUpper v
          mu = read i::Int
parsePass ("}":ts) = (LOOP : fst p, snd p)
    where p = parsePass ts
parsePass (m:i:v:";":ts)
    -- 1 argument instructions
    | iu == "CONCAT" = ((take mu $ repeat $ CONCAT (read v::Int)) ++ (fst p), snd p)
    | iu == "SWAP"   = ((take mu $ repeat $ SWAP   (read v::Int)) ++ (fst p), snd p)
    where p = parsePass ts
          iu = map toUpper i
          mu = read m::Int
parsePass (t:_) = fatalError ("Unknown instruction '"++t++"'")

-- | Parser, constructs Ebel IR from tokens
tokens2Ebel :: [String] -> Ebel
tokens2Ebel [] = []
tokens2Ebel ("W":":":[]) = []
tokens2Ebel ("L":":":[]) = []
tokens2Ebel (pn:":":"{":ts)
    | pn == "W" || pn == "Words" = (WordsPass, fst p) : tokens2Ebel (snd p)
    | pn == "L" || pn == "Lines" = (LinesPass, fst p) : tokens2Ebel (snd p)
    | otherwise = fatalError ("Incorrect pass name '"++pn++"'")
    where
        p = parsePass ts
tokens2Ebel (pn:":":ts)  
    | pn == "W" || pn == "Words" = (WordsPass, fst p) : tokens2Ebel (snd p)
    | pn == "L" || pn == "Lines" = (LinesPass, fst p) : tokens2Ebel (snd p)
    | otherwise = fatalError ("Incorrect pass name '"++pn++"'")
    where
        p = parsePass ts
tokens2Ebel ("{":ts) = (WordsPass, fst p) : tokens2Ebel (snd p)
    where
        p = parsePass ts
tokens2Ebel t = (WordsPass, fst p) : tokens2Ebel (snd p)
    where
        p = parsePass t

checkParen :: [String] -> [String] -> [String]
checkParen (x:xs) []
    | x == "{" || x == "(" || x == "[" = x : checkParen xs [x]
    | x == "}" || x == ")" || x == "]" = fatalError "Mismatched parenthesis"
    | otherwise = x : checkParen xs []
checkParen (x:xs) stk@(t:ts)
    | x == "{" || x == "(" || x == "[" = x : checkParen xs ([x]++stk)
    | x == "}" && t == "{" = x : checkParen xs ts
    | x == ")" && t == "(" = x : checkParen xs ts
    | x == "]" && t == "[" = x : checkParen xs ts
    | x == "}" || x == ")" || x == "]" = fatalError "Mismatched parenthesis"
    | otherwise = x : checkParen xs stk
checkParen [] [] = []
checkParen [] _ = fatalError "Mismatched parenthesis"

-- | Scanner, returns list of tokens
tokenize :: String -> [String]
tokenize t
    | fst s == SSSTART || fst s == SSCOMMENT = checkParen ( snd s ) []     
    | otherwise = fatalError "Missing string terminator"
    where s = splitDelim t ("", SSSTART)

-- | Converts Instruction list into String
pass2String :: [Instruction] -> String
pass2String [] = []
pass2String (i:is) = "  " ++ show i ++ "\n" ++ pass2String is

-- | Converts Ebel IR into String
ebel2String :: Ebel -> String
ebel2String [] = []
ebel2String ((WordsPass, p):ps) = "PASS Words\n" ++ pass2String p ++ ebel2String ps
ebel2String ((LinesPass, p):ps) = "PASS Lines\n" ++ pass2String p ++ ebel2String ps
ebel2String ((ExpressionPass s, p):ps) = "Pass " ++ s ++ " Expression\n" ++ pass2String p ++ ebel2String ps

-- | Compiles Bee string into Ebel string
compile :: String -> String
compile = ebel2String . tokens2Ebel . tokenize

-- | Entry point
-- | Takes the code as an argument
main :: IO()
main = do
    args <- getArgs
    putStr $ compile (head args)

