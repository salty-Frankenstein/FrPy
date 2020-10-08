{-
    language extension: Std
    the standard DSL syntax
-}

module FrPy.Std (
    PyExpr, PyStmt,                 -- types may be used
    pyvar, vI, vF, vB, vS, vL,        -- expression constructors
    pynot, (?||), (?&&), (?==), (?!=), (?<), (?<=), (?>), (?>=),         -- operators
    (?<~), (?<|), (?|>), (?->),
    (?+), (?-), (?*), (?/), (?//), (?%), (?**),
    (?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=),  -- statement operators
    pyif, pyendif, pyelse, pydo, pyignore, pywhile, pyfor, pycall, pyMACRO, -- keywords
    pydef, pyret,
    runScript   -- interface
) where

import FrPy.Core

{- definition of the DSL syntax, some syntactic sugar -}
pyvar = Name . PyName
vI = ValInt
vF = ValFloat
vB = ValBool
vS = ValString
vL = ValList
pynot = PyNot
pycall = Call
pyMACRO = Macro

{- function application, highest priority -}
infixl 9 ?<~
(?<~) :: PyExpr -> PyExpr -> PyExpr
(?<~) f x = Call f [x]

{- pipelines -}
infixr 1 ?<|
(?<|) :: PyExpr -> PyExpr -> PyExpr
(?<|) f x = Call f [x]

infixl 1 ?|>
(?|>) :: PyExpr -> PyExpr -> PyExpr
(?|>) x f = Call f [x]

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

infixr 8 ?**
(?**) :: PyExpr -> PyExpr -> PyExpr
(?**) = PyPow

infix 1 ?=, ?+=, ?-=, ?*=, ?/=, ?//=, ?%=, ?**=
(?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=) :: PyExpr -> PyExpr -> PyStmt
(?=) = Assign 
(?+=) = AssignAdd
(?-=) = AssignSub
(?*=) = AssignMul
(?/=) = AssignDiv
(?//=) = AssignFlrDiv
(?%=) = AssignMod
(?**=) = AssignPow

infixr 2 ?->
(?->) :: PyExpr -> PyExpr -> PyExpr
(?->) = Lambda

data PyKey = Pyend | Pyelse PyStmt

pyif :: PyExpr -> PyStmt -> PyKey -> PyStmt
pyif cond stmt Pyend = IfThen cond stmt
pyif cond stmt1 (Pyelse stmt2) = IfThenElse cond stmt1 stmt2
pyendif = Pyend
pyelse = Pyelse
pydo = Block

pyfor :: (PyStmt, PyExpr, PyStmt) -> PyStmt -> PyStmt
pyfor (init, cond, inc) stmt = For init cond inc stmt
pyignore = Expr
pywhile = While
pydef = Define
pyret = Return

