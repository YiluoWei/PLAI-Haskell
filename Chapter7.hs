module Chapter7 where

data Funcdef = Function String String Expression

data Bind = Binding String Value

type Environment = [Bind]

data Value = NumV Int | Closure String Expression Environment

showValueInt :: Value -> Int
showValueInt v = case v of
    NumV i -> i
    _      -> error"Not Int"

data Expression =
    Num Int |
    Identifier String |
    App Expression Expression |
    Plus Expression Expression |
    Lambda String Expression

lookupEnv :: String -> Environment -> Value
lookupEnv s0 [] = error"Not found in environment"
lookupEnv s0 ((Binding s1 value):bs) = if s0 == s1 then value else lookupEnv s0 bs

interpret :: Expression -> Environment -> Value
interpret exp env = case exp of
    Num i -> NumV i
    Identifier i -> lookupEnv i env
    Plus e1 e2 -> let r1 = (interpret e1 env) in
                  let r2 = (interpret e2 env) in
                  case r1 of
                    Closure _ _ _ -> error"Not a int value in +"
                    NumV i1 -> case r2 of
                        Closure _ _ _ ->  error"Not a int value in +"
                        NumV i2 -> NumV $ i1 + i2
    Lambda id body -> Closure id body env
    App lexp e -> let Closure id body envC = interpret lexp env in
                  let newenv = (Binding id (interpret e env)) : envC in
                  interpret body newenv

f1 = Lambda "x" (Plus (Identifier "x") (Identifier "x"))
f2 =  Lambda "x" (Plus (Identifier "x") (Num 100))
f3 = Lambda "f" (App (Identifier "f") (Num 10))
e0 = App f1 (App f2 (Num 2))
e3 = App f3 $ f1
