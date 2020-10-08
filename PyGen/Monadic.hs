{- 
    language extension: Monadic
    using Monads for do notation
-}

module PyGen.Monadic (
    PyExpr, PyStmtM,                 -- types may be used
    pyvar, vI, vF, vB, vS, vL,        -- expression constructors
    pynot, (?||), (?&&), (?==), (?!=), (?<), (?<=), (?>), (?>=),         -- operators
    (?<~), (?<|), (?|>), (?->),
    (?+), (?-), (?*), (?/), (?//), (?%), (?**),
    (?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=),  -- statement operators
    pyif, pyifelse, pydo, pyignore, pywhile, pyfor, pycall, pyMACRO, -- keywords
    pydef, pyret,
    runScript   -- interface
) where

import PyGen.Core
import PyGen.Std hiding (
    pyif, pyfor, pywhile, pyignore, pydef, pyret, pydo, pyelse,
    (?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=) )
import Control.Monad.Writer

type PyStmtM = Writer [PyStmt] ()

pyif :: PyExpr -> PyStmt -> Writer [PyStmt] ()
pyif cond stmt = writer ((), [IfThen cond stmt])
pyifelse :: PyExpr -> PyStmt -> PyStmt -> Writer [PyStmt] ()
pyifelse cond stmt1 stmt2 = writer ((), [IfThenElse cond stmt1 stmt2])

pyfor :: (PyStmt, PyExpr, PyStmt) -> PyStmt -> Writer [PyStmt] ()
pyfor (init, cond, inc) stmt = writer ((), [For init cond inc stmt])

pywhile :: PyExpr -> PyStmt -> Writer [PyStmt] ()
pywhile cond stmt = writer ((), [While cond stmt])

pyignore :: PyExpr -> Writer [PyStmt] ()
pyignore expr = writer ((), [Expr expr])

pydef :: String -> [PyExpr] -> PyStmt -> Writer [PyStmt] ()
pydef name para stmt = writer ((), [Define name para stmt])

pyret :: PyExpr -> Writer [PyStmt] ()
pyret expr = writer ((), [Return expr])

infix 1 ?=, ?+=, ?-=, ?*=, ?/=, ?//=, ?%=, ?**=
(?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=) :: PyExpr -> PyExpr -> Writer [PyStmt] ()
(?=) name expr = writer ((), [Assign name expr])
(?+=) name expr = writer ((), [AssignAdd name expr])
(?-=) name expr = writer ((), [AssignSub name expr])
(?*=) name expr = writer ((), [AssignMul name expr])
(?/=) name expr = writer ((), [AssignDiv name expr])
(?//=) name expr = writer ((), [AssignFlrDiv name expr])
(?%=) name expr = writer ((), [AssignMod name expr])
(?**=) name expr = writer ((), [AssignPow name expr])

pydo :: Writer [PyStmt] () -> PyStmt
pydo stmt = Block $ snd $ runWriter stmt

