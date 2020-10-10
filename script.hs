--import FrPy.Monadic
import FrPy.Monadic.Clear

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo $ do
        pylet "add" [var "a", var "b", var "c"] (var "a" ?+ var "b" ?+ var "c")
        ignore $ var "print" <| var "add" <~ vI 1 <~ vI 2 <~ vI 3
        var "add2" ?= var "add" <~ vI 0
        ignore $ var "print" <| var "add2" <~ vI 1 <~ vI 2

