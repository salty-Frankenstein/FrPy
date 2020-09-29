import PyGen

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    pydo [
        pyignore $ pycall (var "print") [vi 1,vb True, vs "str", vf 1.23],
        var "a" ?= vi 2,
        pywhile (var "a" ?<= vi 100) $ pydo [
            var "i" ?= vi 2,
            var "f" ?= vb True,
            pywhile (var "i" ?< var "a") $ pydo [
                pyif (var "a" ?% var "i" ?== vi 0) 
                    (var "f" ?= vb False)
                pyend,
                var "i" ?= var "i" ?+ vi 1
            ],
            pyif (var "f")
                (pyignore $ pycall (var "print") [var "a"])
            pyend,
            var "a" ?= var "a" ?+ vi 1
        ]
    ]

