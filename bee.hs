{-# LANGUAGE ViewPatterns #-}

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
              deriving (Show, Eq)

-- | Pass IR
type Pass = (PassType, [Instruction])

-- | Ebel IR
type Ebel = [Pass]

-- | Parsing state to correctly parse strings
data SplitState = SSSTART 
                | SSSTRING 
                deriving (Enum, Show, Eq)

-- | Delimiters, which are ignored by Bee
beeDelims :: [Char]
beeDelims = [' ', '\t']

-- | Symbols recognized by Bee
beeSymbols :: [Char]
beeSymbols = [':', ';', '?', '(', ')', '{', '}']

-- | Function to print error 
fatalError :: String -> a
fatalError msg = error ("ERROR: " ++ msg ++ ".")

-- | Splits string into lexemes
splitDelim :: String -> (String, SplitState) -> (SplitState, [String])
splitDelim (x:xs) (lst, s)
    | s == SSSTRING && x == '"' = (fst $ s3, (lst++[x]) : (snd $ s3))
    | s == SSSTRING = (fst $ s4, (snd $ s4))
    | x == '"' && null lst = (fst $ s5, (snd $ s5))
    | x == '"' = (fst s5, lst : (snd $ s5))
    | x `elem` beeSymbols && null lst = (fst $ s1, [x] : (snd $ s1))
    | x `elem` beeSymbols = (fst $ s1, lst : [x] : (snd $ s1))
    | x `elem` beeDelims && null lst = (fst $ s1, (snd $ s1))
    | x `elem` beeDelims = (fst $ s1, lst : (snd $ s1))
    | otherwise = (fst $ s2, (snd $ s2))
    where
        s1 = splitDelim xs ("", s)
        s2 = splitDelim xs ((lst++[x]), s)
        s3 = splitDelim xs ("", SSSTART)
        s4 = splitDelim xs ((lst++[x]), SSSTRING)
        s5 = splitDelim xs ("\"", SSSTRING)
splitDelim [] ([], s) = (s, [])
splitDelim [] (lst, s) = (s, [lst])

-- | Parses one pass in the code
parsePass :: [String] -> ([Instruction], [String])
parsePass [] = ([], [])
parsePass t@("{":_) = ([], t)
parsePass t@("W":":":_) = ([], t)
parsePass t@("L":":":_) = ([], t)
parsePass ("CONCAT":v:";":ts) = (CONCAT ((read v)::Int) : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("DEL":";":ts) = (DEL : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("LOOP":";":ts) = (LOOP : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("NOP":";":ts) = (NOP : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("SWAP":v:";":ts) = (SWAP ((read v)::Int) : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("}":";":ts) = (LOOP : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("}":ts) = (LOOP : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass (t:_) = fatalError ("Unknown instruction '"++t++"'")

-- | Parser, constructs Ebel IR from tokens
tokens2Ebel :: [String] -> Ebel
tokens2Ebel [] = []
tokens2Ebel ("W":":":[]) = []
tokens2Ebel ("L":":":[]) = []
tokens2Ebel ("W":":":"{":ts) = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel ("L":":":"{":ts) = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel ("W":":":ts) = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel ("{":ts) = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel ("L":":":ts) = (LinesPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel t = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
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
    | fst s == SSSTART = checkParen ( snd s ) []     
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

-- | Compiles Bee string into Ebel string
compile :: String -> String
compile bee = (ebel2String . tokens2Ebel . tokenize) bee

-- | Entry point
-- | Takes the code as an argument
main :: IO()
main = do
    args <- getArgs
    putStr $ compile $ (args!!0)

