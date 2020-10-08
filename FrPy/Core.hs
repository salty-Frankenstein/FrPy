{-
    Core of DSL
    This module is not visible for users
-}

module FrPy.Core (
    PyName(..), PyExpr(..), PyStmt(..),                 -- types may be used
    runScript   -- interface
) where

data PyName = PyName String deriving (Show, Eq)

data PyExpr = Name PyName
    | ValInt Integer
    | ValFloat Float
    | ValBool Bool
    | ValString String
    | ValList [PyExpr]
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
    | Lambda PyExpr PyExpr
    | Macro String              -- interesting macros, can be used in stmts with Expr $ Macro ...
    deriving (Show, Eq)

data PyStmt = Assign PyExpr PyExpr
    | AssignAdd PyExpr PyExpr
    | AssignSub PyExpr PyExpr
    | AssignMul PyExpr PyExpr
    | AssignDiv PyExpr PyExpr
    | AssignFlrDiv PyExpr PyExpr
    | AssignMod PyExpr PyExpr
    | AssignPow PyExpr PyExpr
    | IfThen PyExpr PyStmt
    | IfThenElse PyExpr PyStmt PyStmt
    | While PyExpr PyStmt
    | For PyStmt PyExpr PyStmt PyStmt   -- C-like for-loop statement
    | Define String [PyExpr] PyStmt    -- function definition: name, parameters and body statement
    | Return PyExpr
    | Block [PyStmt]
    | Expr PyExpr
    deriving (Show, Eq)

-- check the expression type for assertions
isName :: PyExpr -> Bool
isName (Name n) = True
isName _ = False

getName :: PyName -> String
getName (PyName n) = n

eval :: PyExpr -> String
eval (Name (PyName n)) = n
eval (ValInt i) = show i
eval (ValFloat f) = show f
eval (ValBool b) = show b
eval (ValString s) = show s
eval (ValList l) = "[" ++ (init $ concat $ map ((++",").eval) l) ++ "]"
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
eval (Lambda x expr) = 
    if isName x then "lambda " ++ eval x ++ ": " ++ eval expr
    else error "lambda function parameter error"
eval (Macro m) = m

genBlock :: PyStmt -> [String]
genBlock block = map ("    "++) $ generate block

{- generate a list of string, each string is a line of the final python code -}
generate :: PyStmt -> [String]
generate (Expr expr) = [eval expr]
generate (Assign name expr) = [eval name ++ " = " ++ eval expr]
generate (AssignAdd name expr) = [eval name ++ " += " ++ eval expr]
generate (AssignSub name expr) = [eval name ++ " -= " ++ eval expr]
generate (AssignMul name expr) = [eval name ++ " *= " ++ eval expr]
generate (AssignDiv name expr) = [eval name ++ " /= " ++ eval expr]
generate (AssignFlrDiv name expr) = [eval name ++ " //= " ++ eval expr]
generate (AssignMod name expr) = [eval name ++ " %= " ++ eval expr]
generate (AssignPow name expr) = [eval name ++ " **= " ++ eval expr]

generate (IfThen cond stmt) = ["if " ++ eval cond ++ ":"] ++ genBlock stmt
generate (IfThenElse cond stmt1 stmt2) =
    ["if " ++ eval cond ++ ":"]
    ++ genBlock stmt1
    ++ ["else:"]
    ++ genBlock stmt2
generate (While cond stmt) = ["while " ++ eval cond ++ ":"] ++ genBlock stmt
generate (For init cond inc stmt) = 
    generate init
    ++ ["while " ++ eval cond ++ ":"]
    ++ genBlock stmt
    ++ genBlock inc

generate (Define name para stmt) = 
    if and $ map isName para then
        ["def " ++ name ++ "(" ++ (init.init.concat.(map ((++", ").eval))) para ++ "):"]
        ++ genBlock stmt
        ++ [""]
    else error "function parameter error"
generate (Return expr) = ["return " ++ eval expr]
generate (Block l) = concat $ map generate l

{- the interface for using this DSL -}
runScript :: PyStmt -> String
runScript = concat.(map (++"\n")).generate

