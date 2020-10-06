import PyGen

defFib :: PyStmt
defFib = pydef "fibTail" [var "n", var "a", var "b"] $ pydo [
        pyif (var "n" ?== vI 1)
            (pyret $ var "a")
        pyend,
        pyret $ pycall (var "fibTail") [var "n" ?- vI 1, var "b", var "a" ?+ var "b"]
    ]

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo [
        defFib,
        pydef "fib" [var "n"] $ pyret $ pycall (var "fibTail") [var "n", vI 1, vI 1],
        pyfor(var "i" ?= vI 1, var "i" ?<= vI 10, var "i" ?+= vI 1) $ pydo [
            pyignore $ var "i" ?|> var "fib" ?|> var "print" -- pipelines
        ],
        var "inc" ?= var "x" ?-> var "x" ?+ vI 1,                   -- lambda function
        var "add" ?= var "x" ?-> var "y" ?-> var "x" ?+ var "y",    -- currying
        pyignore $ pycall (var "print") [pycall (pycall (var "add") [vI 1]) [vI 1]], -- calling
        pyignore $ var "print" ?<| (var "add" ?<| vI 1) ?<| vI 1 -- calling by pipelines
    ]

