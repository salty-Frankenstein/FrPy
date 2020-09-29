data PyExpr = ValInt Integer
    | ValBool Bool
    deriving (Show, Eq)

data PyStmt = Assign String PyExpr
    | IfThen PyExpr PyStmt
    | IfThenElse PyExpr PyStmt PyStmt
    | Block [PyStmt]
    deriving (Show, Eq)

data PyKey = Pyend | Pyelse PyStmt

eval :: PyExpr -> String
eval (ValInt i) = show i
eval (ValBool b) = show b

genBlock :: PyStmt -> String
genBlock block = concat $ map ("    "++) $ generate block

generate :: PyStmt -> [String]
generate (Assign name expr) = [name ++ "=" ++ eval expr]
generate (IfThen cond stmt) = ["if " ++ eval cond ++ ":\n", genBlock stmt]
generate (IfThenElse cond stmt1 stmt2) =
    ["if " ++ eval cond ++ ":\n", 
    genBlock stmt1 ++ "\n",
    "else:\n", 
    genBlock stmt2]
generate (Block l) = map ((++"\n").concat.generate) l

infix 0 =:
(=:) :: String -> PyExpr -> PyStmt
(=:) = Assign 

pyif :: PyExpr -> PyStmt -> PyKey -> PyStmt
pyif cond stmt Pyend = IfThen cond stmt
pyif cond stmt1 (Pyelse stmt2) = IfThenElse cond stmt1 stmt2
pyend = Pyend
pyelse = Pyelse
pydo = Block

main :: IO ()
main = putStrLn.concat.generate $ 
    let ifstmt = pyif (ValBool True) ("a" =: ValInt 2) pyend in
        pydo [
            "a" =: ValInt 1,
            ifstmt,
            pyif (ValBool False)
                ifstmt $ 
            pyelse $
                pydo [
                    "a" =: ValInt 4, 
                    "b" =: ValInt 5
                ]
        ]

