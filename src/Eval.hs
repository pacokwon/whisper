module Eval (eval) where

import Parser (Sexpr (..))

data Value = NumV Int
           | Func ([Value] -> Value)

instance Show Value where
    show (NumV n) = show n
    show (Func _) = "<function>"

getNum :: Value -> Int
getNum (NumV n) = n
getNum x = error $ "Value must be a NumV value. Got: " ++ show x

eval :: Sexpr -> Value
eval (Number n) = NumV n
eval (Ident "+") = Func func
    where
        func = NumV . sum . map getNum
eval (Ident "max") = Func func
    where
        func = NumV . maximum . map getNum
eval (Ident x) = error $ "Unknown identifier " ++ x
eval (Paren (x:xs)) = getFunc (eval x) (map eval xs)
    where
        getFunc (Func f) = f
        getFunc v = error $ "The first element in a list must be a function. Got: " ++ show v
eval (Paren []) = error "List must not be empty!"
