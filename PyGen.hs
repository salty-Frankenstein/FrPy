module PyGen where

data PyExpr = Name String
    | ValInt Integer
    | ValFloat Float
    | ValBool Bool
    | ValString String
    | PyAnd PyExpr PyExpr
    | PyOr PyExpr PyExpr
    | PyNot PyExpr
    | PyGr PyExpr PyExpr
    | PyGe PyExpr PyExpr
    | PyLs PyExpr PyExpr
    | PyLe PyExpr PyExpr
    | PyEq PyExpr PyExpr
    | PyNeq PyExpr PyExpr
    | PyAdd PyExpr PyExpr
    | PySub PyExpr PyExpr
    | PyMul PyExpr PyExpr
    | PyDiv PyExpr PyExpr
    | PyFlrDiv PyExpr PyExpr
    | PyMod PyExpr PyExpr
    | PyPow PyExpr PyExpr
    deriving (Show, Eq)

var = Name
vi = ValInt
vf = ValFloat
vb = ValBool
vs = ValString
pynot = PyNot

infixr 2 ?||
(?||) :: PyExpr -> PyExpr -> PyExpr
(?||) = PyOr

infixr 3 ?&&
(?&&) :: PyExpr -> PyExpr -> PyExpr
(?&&) = PyAnd

infix ?==, ?!=, ?<, ?<=, ?>, ?>=
(?==), (?!=), (?<), (?<=), (?>), (?>=) :: PyExpr -> PyExpr -> PyExpr
(?==) = PyEq
(?!=) = PyNeq
(?<) = PyLs
(?<=) = PyLe
(?>) = PyGr
(?>=) = PyGe

infixl 6 ?+, ?-
(?+), (?-) :: PyExpr -> PyExpr -> PyExpr
(?+) = PyAdd
(?-) = PySub

infixl 7 ?*, ?/, ?//, ?%
(?*), (?/), (?//), (?%) :: PyExpr -> PyExpr -> PyExpr
(?*) = PyMul
(?/) = PyDiv
(?//) = PyFlrDiv
(?%) = PyMod

data PyStmt = Assign PyExpr PyExpr
    | IfThen PyExpr PyStmt
    | IfThenElse PyExpr PyStmt PyStmt
    | Call PyExpr [PyExpr]      -- function call: function name and arguments
    | Block [PyStmt]
    deriving (Show, Eq)

data PyKey = Pyend | Pyelse PyStmt

eval :: PyExpr -> String
eval (Name n) = n
eval (ValInt i) = show i
eval (ValFloat f) = show f
eval (ValBool b) = show b
eval (ValString s) = show s
eval (PyAdd e1 e2) = eval e1 ++ " + " ++ eval e2
eval (PySub e1 e2) = eval e1 ++ " - " ++ eval e2
eval (PyMul e1 e2) = eval e1 ++ " * " ++ eval e2
eval (PyDiv e1 e2) = eval e1 ++ " / " ++ eval e2
eval (PyFlrDiv e1 e2) = eval e1 ++ " // " ++ eval e2
eval (PyMod e1 e2) = eval e1 ++ " % " ++ eval e2
eval (PyAnd e1 e2) = eval e1 ++ " and " ++ eval e2
eval (PyOr e1 e2) = eval e1 ++ " or " ++ eval e2
eval (PyNot e) = " not " ++ eval e
eval (PyEq e1 e2) = eval e1 ++ " == " ++ eval e2
eval (PyNeq e1 e2) = eval e1 ++ " != " ++ eval e2
eval (PyLs e1 e2) = eval e1 ++ " < " ++ eval e2
eval (PyLe e1 e2) = eval e1 ++ " <= " ++ eval e2
eval (PyGr e1 e2) = eval e1 ++ " > " ++ eval e2
eval (PyGe e1 e2) = eval e1 ++ " >= " ++ eval e2

genBlock :: PyStmt -> String
genBlock block = concat $ map (("    "++).(++"\n")) $ generate block

generate :: PyStmt -> [String]
generate (Assign name expr) = [eval name ++ " = " ++ eval expr]
generate (IfThen cond stmt) = ["if " ++ eval cond ++ ":", genBlock stmt]
generate (IfThenElse cond stmt1 stmt2) =
    ["if " ++ eval cond ++ ":", 
    genBlock stmt1,
    "else:",
    genBlock stmt2]
generate (Call f arg) = [eval f ++ "(" ++ (init.init $ concat $ map ((++", ").eval) arg) ++ ")"]
generate (Block l) = map (concat.(map (++"\n")).generate) l

removeLn ::  Bool -> String -> String
removeLn _ [] = []
removeLn False (x:xs) = if x /= '\n' then x : removeLn False xs
    else x: removeLn True xs
removeLn True (x:xs) = if x /= '\n' then x : removeLn False xs
    else removeLn True xs

runScript :: PyStmt -> String
runScript = concat.(map $ (++"\n").(removeLn False)).generate

infix 0 ?=
(?=) :: PyExpr -> PyExpr -> PyStmt
(?=) = Assign 

pyif :: PyExpr -> PyStmt -> PyKey -> PyStmt
pyif cond stmt Pyend = IfThen cond stmt
pyif cond stmt1 (Pyelse stmt2) = IfThenElse cond stmt1 stmt2
pyend = Pyend
pyelse = Pyelse
pydo = Block

