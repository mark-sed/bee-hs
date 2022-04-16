module Main where

import Data.Maybe

data Instruction = CONCAT Int
                 | DEL
                 | LOOP
                 | NOP
                 | SWAP Int
                 deriving Show

data PassType = WordsPass | LinesPass deriving (Show, Eq)

type Pass = (PassType, [Instruction])

type Ebel = [Pass]

data SplitState = SSSTART | SSSTRING deriving (Enum, Show, Eq)

beeDelims = [' ', '\t']
beeSymbols = [':', ';', '?', '(', ')', '{', '}']

fatalError msg = error ("ERROR: " ++ msg ++ ".")

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

parseInst :: [String] -> (Instruction, [String])
parseInst ("CONCAT":v:";":ts) = (CONCAT ((read v)::Int), ts)
parseInst ("NOP":";":ts) = (NOP, ts)
parseInst (t:_) = fatalError ("Unknown instruction '"++t++"'")

parsePass :: [String] -> ([Instruction], [String])
parsePass [] = ([], [])
parsePass t@("W":":":ts) = ([], t)
parsePass t@("L":":":ts) = ([], t)
parsePass t = ((fst $ p) : (fst $ parsePass (snd $ p)), (snd $ p))
    where p = parseInst t

tokens2Ebel :: [String] -> Ebel
tokens2Ebel [] = []
tokens2Ebel ("W":":":[]) = []
tokens2Ebel ("L":":":[]) = []
tokens2Ebel ("W":":":ts) = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel ("L":":":ts) = (LinesPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass ts
tokens2Ebel t = (WordsPass, (fst $ p)) : tokens2Ebel (snd $ p)
    where
        p = parsePass t

tokenize :: String -> [String]
tokenize t
    | fst s == SSSTART = snd s
    | otherwise = fatalError "Missing string terminator"
    where s = splitDelim t ("", SSSTART)

y' s = tokens2Ebel $ tokenize s

compile :: String -> Maybe String
compile bee = Just "Bzzz"

main :: IO()
main = putStrLn "Bzzz!"
