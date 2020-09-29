module PyGen (
    var, vi, vf, vb, vs,        -- expression constructors
    pynot, (?||), (?&&), (?==), (?!=), (?<), (?<=), (?>), (?>=),         -- operators
    (?+), (?-), (?*), (?/), (?//), (?%),
    (?=),   -- statement operators
    pyif, pyend, pyelse, pydo, pyignore, pywhile, pycall, -- keywords
    runScript   -- interface
) where

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
    | Call PyExpr [PyExpr]      -- function call: function name and arguments
    deriving (Show, Eq)

data PyStmt = Assign PyExpr PyExpr
    | IfThen PyExpr PyStmt
    | IfThenElse PyExpr PyStmt PyStmt
    | While PyExpr PyStmt
    | Block [PyStmt]
    | Expr PyExpr
    deriving (Show, Eq)

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
eval (Call f arg) = eval f ++ "(" ++ (init.init $ concat $ map ((++", ").eval) arg) ++ ")"

genBlock :: PyStmt -> [String]
genBlock block = map ("    "++) $ generate block

{- generate a list of string, each string is a line of the final python code -}
generate :: PyStmt -> [String]
generate (Expr expr) = [eval expr]
generate (Assign name expr) = [eval name ++ " = " ++ eval expr]
generate (IfThen cond stmt) = ["if " ++ eval cond ++ ":"] ++ genBlock stmt
generate (IfThenElse cond stmt1 stmt2) =
    ["if " ++ eval cond ++ ":"]
    ++ genBlock stmt1
    ++ ["else:"]
    ++ genBlock stmt2
generate (While cond stmt) = ["while " ++ eval cond ++ ":"] ++ genBlock stmt
generate (Block l) = concat $ map generate l

{- the interface for using this DSL -}
runScript :: PyStmt -> String
runScript = concat.(map (++"\n")).generate

{- definition of the DSL syntax, some syntactic sugar -}
var = Name
vi = ValInt
vf = ValFloat
vb = ValBool
vs = ValString
pynot = PyNot
pycall = Call

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


infix 0 ?=
(?=) :: PyExpr -> PyExpr -> PyStmt
(?=) = Assign 

data PyKey = Pyend | Pyelse PyStmt

pyif :: PyExpr -> PyStmt -> PyKey -> PyStmt
pyif cond stmt Pyend = IfThen cond stmt
pyif cond stmt1 (Pyelse stmt2) = IfThenElse cond stmt1 stmt2
pyend = Pyend
pyelse = Pyelse
pydo = Block
pyignore = Expr
pywhile = While


