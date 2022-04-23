module Parser where

data Instruction = CONCAT Int
                 | DEL
                 | LOOP
                 | NOP
                 | SWAP Int
                 | ADD String String String
                 | DIV String String String
                 | MOD String String String
                 | MOVE String String String
                 | MUL String String String
                 | POW String String String
                 | SUB String String String
                 deriving (Show, Eq)

data Pragma = Sym_table_size Int
            | Requires String
            deriving (Eq)

instance Show Pragma where
    show (Sym_table_size a) = "@pragma sym_table_size "++show a
    show (Requires a) = "@pragma requires "++a

data PassType = WordsPass
              | LinesPass
              | ExpressionPass String
              deriving (Show, Eq)

type Pass = (PassType, [Instruction])

type Ebel = [Pass]

type EbelProgram = ([Pragma], Ebel)

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

genVar :: Int -> String
genVar v = "$"++(show v)

expr2Ebel :: Int -> Expr -> Either String (String, [Instruction])
    -- ADD
expr2Ebel v (EADD Var Var) = Right (show v, [ADD (genVar v) "$" "$"])
expr2Ebel v (EADD Var (IVal a)) = Right (show v, [ADD (genVar v) "$" (show a)])
expr2Ebel v (EADD (IVal a) Var) = Right (show v, [ADD (genVar v) (show a) "$"])
expr2Ebel v (EADD (IVal a) (IVal b)) = Right (show v, [ADD (genVar v) (show a) (show b)])
    -- Var
expr2Ebel v Var = Right (show v, [MOVE "$" "$" "$"])
    -- Nested

str2Expr :: String -> Either String Expr
str2Expr [] = Left "Empty expression"
 









