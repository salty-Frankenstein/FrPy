import FrPy

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo [
        pyignore $ pycall (var "print") [vI 1,vB True, vS "str", vF 1.23],
        var "l" ?= vL [vI 1, vB True, vL [vF 3.141, vF 1.414], vS "hello"],
        pyignore $ pyMACRO "print(l)",
        var "a" ?= vI 2,
        pywhile (var "a" ?<= vI 100) $ pydo [
            var "i" ?= vI 2,
            var "f" ?= vB True,
            pywhile (var "i" ?< var "a") $ pydo [
                pyif (var "a" ?% var "i" ?== vI 0) 
                    (var "f" ?= vB False)
                pyend,
                var "i" ?+= vI 1
            ],
            pyif (var "f")
                (pyignore $ pycall (var "print") [var "a"])
            pyend,
            var "a" ?+= vI 1
        ]
    ]

