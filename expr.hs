{-# OPTIONS_GHC -Wall #-}

data Expr = Const Int
        | Var String 
        | Binary Op Expr Expr

data Op = Plus
        | Minus
        | Mul    
        | Div

data Error = NoVar String | ZeroDivision deriving Show

type Env = [(String, Int)]

main :: IO()
main = do
    print (evalExpr [("a", 5), ("b", 3), ("c", 10), ("d", 100)] (Binary Mul (Binary Plus (Var "a") (Var "b")) (Binary Div (Var "d") (Var "c")))) --ответ - 80
    putStr ""
    print (evalExpr [("q", 0), ("w", 3), ("e", 10), ("r", 100)] (Binary Mul (Binary Plus (Var "a") (Var "b")) (Binary Div (Var "w") (Var "q")))) --тест ошибок

evalExpr :: Env -> Expr -> Either [Error] Int
evalExpr _ (Const x) = Right x
evalExpr l (Var x) = 
    case (lookup x l) of 
        Nothing -> Left [NoVar x]
        Just y  -> Right y

evalExpr env (Binary op e1 e2) =
        case (res1, res2) of
            (Right n1, Right n2)    -> makeOp op n1 n2
            (Left err1, Left err2)  -> Left (err1 ++ err2)
            (Left err, _)           -> Left err
            (_, err)                -> err
        where
            res1 = evalExpr env e1
            res2 = evalExpr env e2

makeOp :: Op -> Int -> Int -> Either [Error] Int
makeOp Plus x y  = Right (x + y)
makeOp Minus x y = Right (x - y)
makeOp Mul x y   = Right (x * y)
makeOp Div x y | y == 0 = Left [ZeroDivision]
               | otherwise = Right (x `div` y)






