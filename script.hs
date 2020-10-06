import PyGen.Clear

defFib :: PyStmt
defFib = def "fibTail" [var "n", var "a", var "b"] $ pydo [
        pyif (var "n" ?== vI 1)
            (ret $ var "a")
        endif,
        ret $ call (var "fibTail") [var "n" ?- vI 1, var "b", var "a" ?+ var "b"]
    ]

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo [
        defFib,
        def "fib" [var "n"] $ ret $ call (var "fibTail") [var "n", vI 1, vI 1],
        for(var "i" ?= vI 1, var "i" ?<= vI 10, var "i" ?+= vI 1) $ pydo [
            ignore $ var "i" |> var "fib" |> var "print" -- pipelines
        ],
        var "inc" ?= var "x" --> var "x" ?+ vI 1,                   -- lambda function
        var "add" ?= var "x" --> var "y" --> var "x" ?+ var "y",    -- currying
        call_ (var "print") [call (call (var "add") [vI 1]) [vI 1]], -- calling
        ignore $ var "print" <| (var "add" <| vI 1) <| vI 1 -- calling by pipelines
    ]

