--import PyGen.Monadic
import PyGen.Monadic.Clear

defFib :: PyStmtM
defFib = def "fibTail" [var "n", var "a", var "b"] $ 
    pydo $ do
        pyif (var "n" ?== vI 1) $ pydo $ do 
            ret $ var "a"
        ret $ call (var "fibTail") [var "n" ?- vI 1, var "b", var "a" ?+ var "b"]

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo $ do
        defFib
        def "fib" [var "n"] $ pydo $ do
            ret $ call (var "fibTail") [var "n", vI 1, vI 1]
        
        for(pydo $ var "i" ?= vI 1, var "i" ?<= vI 10, pydo $ var "i" ?+= vI 1) $ pydo $ do
            ignore $ var "i" |> var "fib" |> var "print" -- pipelines
        
        var "inc" ?= var "x" --> var "x" ?+ vI 1                   -- lambda function
        var "add" ?= var "x" --> var "y" --> var "x" ?+ var "y"    -- currying
        ignore $ call (var "print") [call (call (var "add") [vI 1]) [vI 1]] -- pycalling
        ignore $ var "print" <| var "add" <~ vI 1 <~ vI 1
        
