{- |
Module     : Parser.hs
Description: Bee language parser
Author     : Marek Sedlacek
Email      : mr.mareksedlacek@gmail.com
Date       : April 2022
Copyright  : 2022 Marek Sedlacek. All rights reserved.

Parser for bee language
-}

module Parser where

import Utils
import Data.Either

-- | All Ebel instructions
data Instruction = CONCAT Int
                 | DEL
                 | LOOP
                 | NOP
                 | SWAP Int
                 | ADD String String String
                 | DIV String String String
                 | MOD String String String
                 | MOVE String String
                 | MUL String String String
                 | POW String String String
                 | SUB String String String
                 deriving (Eq)
instance Show Instruction where
    show (CONCAT a) = "CONCAT "++show a
    show LOOP = "LOOP"
    show NOP = "NOP"
    show (SWAP a) = "SWAP "++show a
    show (ADD d s1 s2) = "ADD "++d++", "++s1++", "++s2
    show (DIV d s1 s2) = "DIV "++d++", "++s1++", "++s2
    show (MOD d s1 s2) = "MOD "++d++", "++s1++", "++s2
    show (MOVE d s1) = "MOVE "++d++", "++s1
    show (MUL d s1 s2) = "MUL "++d++", "++s1++", "++s2
    show (POW d s1 s2) = "POW "++d++", "++s1++", "++s2
    show (SUB d s1 s2) = "SUB "++d++", "++s1++", "++s2

-- | All Ebel pragmas
data Pragma = Sym_table_size Int
            | Requires String
            deriving (Eq)
instance Show Pragma where
    show (Sym_table_size a) = "@pragma sym_table_size "++show a
    show (Requires a) = "@pragma requires "++a

-- | Ebel data types excluding internal ones
data Type = TEXT
          | NUMBER
          | FLOAT
          | DELIMITER
          | SYMBOL
          | EMPTY
          | DERIVED
          deriving Show

-- | Internal pass types
data PassType = WordsPass
              | WordsPassCont
              | LinesPass
              | ExpressionPass Type
              | MatchPass String
instance Show PassType where
    show WordsPass = "Pass Words"
    show WordsPassCont = "WordsPassCont"
    show LinesPass = "Pass Lines"
    show (ExpressionPass t) = "Pass " ++ show t ++ " Expression"
    show (MatchPass s) = "Pass \"" ++ s ++ "\" Expression"

-- | Ebel Pass
type Pass = (PassType, [Instruction])

-- | Just Passes
type Ebel = [Pass]

-- | Full Ebel program with pragmas and passes
type EbelProgram = ([Pragma], Ebel)

-- | AST for user defined expressions
data Expr = Var
          | IVal Int
          | FVal Float
          | EADD Expr Expr
          | EDIV Expr Expr
          | EMOD Expr Expr
          | EMUL Expr Expr
          | EPOW Expr Expr
          | ESUB Expr Expr
          deriving (Show)

-- | Creates variable based on its index
genVar :: Int -> String
genVar v = "$" ++ show v

-- TODO: Optimize 2 IVals in expression

-- | Converts AST expression into Ebel code
-- | The first arguments is the starting variable (0)
-- | If left is returned or Right with negative Int, then the expression is incorrect
expr2Ebel :: Int -> Expr -> Either String (Int, [Instruction])
    -- ADD
expr2Ebel v (EADD Var Var) = Right (v, [ADD (genVar v) "$" "$"])
expr2Ebel v (EADD Var (IVal a)) = Right (v, [ADD (genVar v) "$" (show a)])
expr2Ebel v (EADD (IVal a) Var) = Right (v, [ADD (genVar v) (show a) "$"])
expr2Ebel v (EADD (IVal a) (IVal b)) = Right (v, [ADD (genVar v) (show a) (show b)])
expr2Ebel v (EADD a (IVal b)) = Right (v, snd ar ++ [ADD (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EADD a Var) = Right (v, snd ar ++ [ADD (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EADD (IVal a) b) = Right (v, snd br ++ [ADD (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EADD Var b) = Right (v, snd br ++ [ADD (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EADD a b) = Right (v, snd ar ++ snd br ++ [ADD (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- DIV
expr2Ebel v (EDIV Var Var) = Right (v, [DIV (genVar v) "$" "$"])
expr2Ebel v (EDIV Var (IVal a)) = Right (v, [DIV (genVar v) "$" (show a)])
expr2Ebel v (EDIV (IVal a) Var) = Right (v, [DIV (genVar v) (show a) "$"])
expr2Ebel v (EDIV (IVal a) (IVal b)) = Right (v, [DIV (genVar v) (show a) (show b)])
expr2Ebel v (EDIV a (IVal b)) = Right (v, snd ar ++ [DIV (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EDIV a Var) = Right (v, snd ar ++ [DIV (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EDIV (IVal a) b) = Right (v, snd br ++ [DIV (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EDIV Var b) = Right (v, snd br ++ [DIV (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EDIV a b) = Right (v, snd ar ++ snd br ++ [DIV (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- MOD
expr2Ebel v (EMOD Var Var) = Right (v, [MOD (genVar v) "$" "$"])
expr2Ebel v (EMOD Var (IVal a)) = Right (v, [MOD (genVar v) "$" (show a)])
expr2Ebel v (EMOD (IVal a) Var) = Right (v, [MOD (genVar v) (show a) "$"])
expr2Ebel v (EMOD (IVal a) (IVal b)) = Right (v, [MOD (genVar v) (show a) (show b)])
expr2Ebel v (EMOD a (IVal b)) = Right (v, snd ar ++ [MOD (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EMOD a Var) = Right (v, snd ar ++ [MOD (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EMOD (IVal a) b) = Right (v, snd br ++ [MOD (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EMOD Var b) = Right (v, snd br ++ [MOD (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EMOD a b) = Right (v, snd ar ++ snd br ++ [MOD (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- MUL
expr2Ebel v (EMUL Var Var) = Right (v, [MUL (genVar v) "$" "$"])
expr2Ebel v (EMUL Var (IVal a)) = Right (v, [MUL (genVar v) "$" (show a)])
expr2Ebel v (EMUL (IVal a) Var) = Right (v, [MUL (genVar v) (show a) "$"])
expr2Ebel v (EMUL (IVal a) (IVal b)) = Right (v, [MUL (genVar v) (show a) (show b)])
expr2Ebel v (EMUL a (IVal b)) = Right (v, snd ar ++ [MUL (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EMUL a Var) = Right (v, snd ar ++ [MUL (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EMUL (IVal a) b) = Right (v, snd br ++ [MUL (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EMUL Var b) = Right (v, snd br ++ [MUL (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EMUL a b) = Right (v, snd ar ++ snd br ++ [MUL (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- POW
expr2Ebel v (EPOW Var Var) = Right (v, [POW (genVar v) "$" "$"])
expr2Ebel v (EPOW Var (IVal a)) = Right (v, [POW (genVar v) "$" (show a)])
expr2Ebel v (EPOW (IVal a) Var) = Right (v, [POW (genVar v) (show a) "$"])
expr2Ebel v (EPOW (IVal a) (IVal b)) = Right (v, [POW (genVar v) (show a) (show b)])
expr2Ebel v (EPOW a (IVal b)) = Right (v, snd ar ++ [POW (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EPOW a Var) = Right (v, snd ar ++ [POW (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (EPOW (IVal a) b) = Right (v, snd br ++ [POW (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EPOW Var b) = Right (v, snd br ++ [POW (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (EPOW a b) = Right (v, snd ar ++ snd br ++ [POW (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- SUB
expr2Ebel v (ESUB Var Var) = Right (v, [SUB (genVar v) "$" "$"])
expr2Ebel v (ESUB Var (IVal a)) = Right (v, [SUB (genVar v) "$" (show a)])
expr2Ebel v (ESUB (IVal a) Var) = Right (v, [SUB (genVar v) (show a) "$"])
expr2Ebel v (ESUB (IVal a) (IVal b)) = Right (v, [SUB (genVar v) (show a) (show b)])
expr2Ebel v (ESUB a (IVal b)) = Right (v, snd ar ++ [SUB (genVar v) (genVar (fst ar)) (show b)])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (ESUB a Var) = Right (v, snd ar ++ [SUB (genVar v) (genVar (fst ar)) "$"])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
expr2Ebel v (ESUB (IVal a) b) = Right (v, snd br ++ [SUB (genVar v) (show a) (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (ESUB Var b) = Right (v, snd br ++ [SUB (genVar v) "$" (genVar (fst br))])
    where
        br = fromRight (-1, []) $ expr2Ebel (v+1) b
expr2Ebel v (ESUB a b) = Right (v, snd ar ++ snd br ++ [SUB (genVar v) (genVar (fst ar)) (genVar (fst br))])
    where
        ar = fromRight (-1, []) $ expr2Ebel (v+1) a
        br = fromRight (-1, []) $ expr2Ebel (v+2) b
    -- Var
expr2Ebel v Var = Right (v, [MOVE "$" "$"])
    -- Val
expr2Ebel v (IVal a) = Right (v, [MOVE "$" (show a)])
expr2Ebel _ _ = Left "Incorrect expression"

isBeeDelim :: Char -> Bool
isBeeDelim a = a `elem` [' ', '\n', '\t']

isBeeOperator :: Char -> Bool
isBeeOperator a = a `elem` [';', '?', ':', '+', '-', '/', '%', '^', '(', ')', '{', '}', '[', ']']

getTokenStr :: String -> String -> Either String (String, String)
getTokenStr [] _ = Left "Missing closing quote symbol"
getTokenStr ('"':xs) s = Right ('"':s++['"'], xs)
getTokenStr (x:xs) s = getTokenStr xs $ s++[x]


-- TODO: Error does not get propagated
tokenize :: String -> Either String [String]
tokenize [] = Right []
tokenize code
    | isBeeDelim (head code) = tokenize (tail code)
    | isBeeOperator (head code) = Right (fst s1 : (fromRight ["ERROR1"] (tokenize (snd s1))))
    | head code == '"' = if isLeft s3 then Left (fromLeft "ERROR2" s3) else Right (fst s4 : (fromRight ["ERROR3"] (tokenize (snd s4))))
    | otherwise = Right (fst s2 : (fromRight ["ERROR4"] (tokenize (snd s2))))
    where s1 = span (\a -> isBeeOperator a) code
          s2 = span (\a -> not ((isBeeDelim a) || (isBeeOperator a))) code
          s3 = getTokenStr (tail code) ""
          s4 = fromRight ("ERROR5", "ERROR6") s3
          

-- TODO: Parse expression to list of strings
--       Convert the list into prefix notation
--       Tree can be then generated 

-- | Parses string expression into AST
-- | If left is returned, then Error was encountered
str2Expr :: String -> Either String Expr
str2Expr "" = Left "Empty expression"

-- | Converts instructions into formatted string
-- | First argument is the number of spaces to use before instruction
instr2StrT :: Int -> [Instruction] -> String
instr2StrT _ [] = ""
instr2StrT tab (i:is) = replicate tab ' ' ++ show i ++ "\n" ++ instr2StrT tab is

-- | Converts Pass into formatted string
-- | First argument is the number of spaces to use before instruction
pass2StrT :: Int -> Pass -> String
pass2StrT tab (ExpressionPass t, i) = replicate (tab+2) ' ' ++ show (ExpressionPass t) ++ "\n" ++ instr2StrT (tab+4) (init i) ++ replicate (tab+4) ' ' ++ "RETURN " ++ show (last i) ++ "\n"
pass2StrT tab (MatchPass s, i) = replicate (tab+2) ' ' ++ show (MatchPass s) ++ "\n" ++ instr2StrT (tab+4) (init i) ++ replicate (tab+4) ' ' ++ "RETURN " ++ show (last i) ++ "\n"
pass2StrT tab (WordsPassCont, i) = replicate tab ' ' ++ instr2StrT (tab+2) i
pass2StrT tab (p, i) = replicate tab ' ' ++ show p ++ "\n" ++ instr2StrT (tab+2) i

-- | Converts Pragmas into string to prepend to program
pragmas2Str :: [Pragma] -> String
pragmas2Str [] = ""
pragmas2Str (p:ps) = show p ++ "\n" ++ pragmas2Str ps

-- | Converts Passes into formatted string
pass2Str :: [Pass] -> String
pass2Str [] = ""
pass2Str (p:ps) = pass2StrT 0 p ++ pass2Str ps

-- | Converts Ebel IR into valid Ebel program
-- | Adds comment with bee-hs version at the first line
--   Pragma requires 0.3.1 is also added (for match expr and FLOATs)
ebel2Str :: EbelProgram -> String 
ebel2Str (pr, ps) = "# Generated by " ++ bee_hs_name ++ "\n" ++ pragmas2Str (Requires "0.3.1":pr) ++ pass2Str ps


-- TODO: Remove
exampleEbel = ([], [(WordsPass, [])]++[(MatchPass "42", (snd $ fromRight (-1, []) (expr2Ebel 0 (EMUL (EDIV (IVal 42) Var) (EADD (EPOW (IVal 8) (IVal 8)) (IVal 10)))))++[NOP])]++[(WordsPassCont, [LOOP])])
