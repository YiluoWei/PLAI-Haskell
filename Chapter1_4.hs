module Chapter1_4 where


data ArithC = Num Int | Plus ArithC ArithC | Mult ArithC ArithC

interpretArithC :: ArithC -> Int
interpretArithC a = case a of
    Num i -> i
    Plus l r -> (interpretArithC l) + (interpretArithC r)
    Mult l r -> (interpretArithC l) + (interpretArithC r)


data ArithS =
    NumS Int |
    PlusS ArithS ArithS |
    BMinus ArithS ArithS |
    MultS ArithS ArithS |
    UMinus ArithS

desugarArithS :: ArithS -> ArithC
desugarArithS a = case a of
    NumS i -> Num i
    PlusS l r -> Plus (desugarArithS l) (desugarArithS r)
    MultS l r -> Mult (desugarArithS l) (desugarArithS r)
    BMinus l r -> Plus (desugarArithS l) (Mult (Num (-1)) (desugarArithS r))
    UMinus l -> Mult (Num (-1)) (desugarArithS l)



