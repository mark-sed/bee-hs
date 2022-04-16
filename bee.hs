module Main where

import System.Environment

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

beeDelims :: [Char]
beeDelims = [' ', '\t']

beeSymbols :: [Char]
beeSymbols = [':', ';', '?', '(', ')', '{', '}']

fatalError :: String -> a
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

parsePass :: [String] -> ([Instruction], [String])
parsePass [] = ([], [])
parsePass t@("W":":":_) = ([], t)
parsePass t@("L":":":_) = ([], t)
parsePass ("CONCAT":v:";":ts) = (CONCAT ((read v)::Int) : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass ("NOP":";":ts) = (NOP : (fst $ p), (snd $ p))
    where p = parsePass ts
parsePass (t:_) = fatalError ("Unknown instruction '"++t++"'")

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

pass2String :: [Instruction] -> String
pass2String [] = []
pass2String (i:is) = "  " ++ show i ++ "\n" ++ pass2String is

ebel2String :: Ebel -> String
ebel2String [] = []
ebel2String ((WordsPass, p):ps) = "PASS Words\n" ++ pass2String p ++ ebel2String ps
ebel2String ((LinesPass, p):ps) = "PASS Lines\n" ++ pass2String p ++ ebel2String ps

compile :: String -> String
compile bee = (ebel2String . tokens2Ebel . tokenize) bee

main :: IO()
main = do
    args <- getArgs
    putStr $ compile $ (args!!0)

