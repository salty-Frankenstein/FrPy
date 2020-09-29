import PyGen

main :: IO ()
main = writeFile "script.py" $ runScript $ 
    let ifstmt = pyif (var "a" ?== vi 1) (var "a" ?= vi 2) pyend in
        pydo [
            Call (var "print") [vi 1,vb True, vs "str", vf 1.23],
            var "a" ?= vi 1,
            ifstmt,
            pyif (var "a" ?>= vi 0 ?&& pynot (vb True))
                ifstmt $ 
            pyelse $
                pydo [
                    var "a" ?= vi 4, 
                    var "b" ?= vf 3.14,
                    var "c" ?= var "a" ?+ var "b" ?* vf 4.7
                ]
        ]

