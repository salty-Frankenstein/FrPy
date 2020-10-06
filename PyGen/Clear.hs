{- 
    language extension: Clear
    for simpler keywords and operators
-}
module PyGen.Clear (
    PyExpr, PyStmt,                 -- types may be used
    var, vI, vF, vB, vS, vL,        -- expression constructors
    pynot, (?||), (?&&), (?==), (?!=), (?<), (?<=), (?>), (?>=),         -- operators
    (<|), (|>), (-->),
    (?+), (?-), (?*), (?/), (?//), (?%), (?**),
    (?=), (?+=), (?-=), (?*=), (?/=), (?//=), (?%=), (?**=),  -- statement operators
    pyif, endif, pyelse, pydo, ignore, while, for, call, call_, pyMACRO, -- keywords
    def, ret,
    runScript   -- interface
) where

import PyGen.Std

call = pycall
ignore = pyignore

call_ :: PyExpr -> [PyExpr] -> PyStmt
call_ x y = pyignore $ pycall x y

for = pyfor
while = pywhile
def = pydef
ret = pyret
var = pyvar
endif = pyendif
infixr 1 <|
(|>) = (?|>)
infixl 1 |>
(<|) = (?<|)
infixr 1 -->
(-->) = (?->)

