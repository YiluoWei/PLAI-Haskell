module Chapter8 where

data Expression =
    Num Int |
    Var String |
    App Expression Expression |
    Plus Expression Expression |
    Lambda String Expression |
    Set String Expression |
    Seq Expression Expression

data Value = NumV Int | Closure String Expression Environment

data Funcdef = Function String String Expression

data Bind = Binding String Value

type Environment = [Bind]

data Storage = Str Int Value

type Store = [Storage]

type Result = (Value, Store)

lookupEnv :: String -> Environment -> Value
lookupEnv s0 [] = error"Not found in environment"
lookupEnv s0 ((Binding s1 value):bs) = if s0 == s1 then value else lookupEnv s0 bs

overRideStore :: Store -> Storage -> Store
overRideStore [] str = [str]
overRideStore ((Str i1 v1):ss) (Str i2 v2) = if i1 == i2 then (Str i1 v2):ss else overRideStore ss (Str i2 v2)

fetchStore :: Store -> Int -> Value
fetchStore [] _ = error"Nothing found in store"
fetchStore (s:ss) loc = let (Str i v) = s in if i == loc then v else fetchStore ss loc

valuePlus :: Value -> Value -> Value
valuePlus (Closure _ _ _) _ = error"Not addable"
valuePlus _ (Closure _ _ _) = error"Not addable"
valuePlus (NumV i1) (NumV i2) = NumV (i1 + i2)

interpret :: Expression -> Environment -> Store -> Result
interpret exp env sto = case exp of
    Set var e -> let r1 = interpret e env sto in
                 let e_value = fst r1 in
                 let (NumV loc) = lookupEnv var env in
                 let newstore = overRideStore sto (Str loc e_value) in
                 (e_value, newstore)
    Num i -> (NumV i, sto)
    Lambda id e -> (Closure id e env, sto)
    Var var -> let (NumV loc) = lookupEnv var env in
               let value = fetchStore sto loc in
               (value, sto)
    Seq e1 e2 -> let (new_value, new_sto) = interpret e1 env sto in
                 interpret e2 env new_sto
    Plus e1 e2 -> let (e1_value, e1_sto) = interpret e1 env sto in
                  let (e2_value, e2_sto) = interpret e2 env e1_sto in
                  let value = e1_value `valuePlus` e2_value in
                  (value, e2_sto)
    App e1 e2 -> let (Closure e1_id e1_exp e1_env, e1_sto) = interpret e1 env sto in
                 let (e2_value, e2_sto) = interpret e2 env e1_sto in
                 let num = length e2_sto in
                 let new_env = (Binding e1_id (NumV num)) : e1_env in
                 let new_sto = overRideStore e2_sto (Str num e2_value) in
                 interpret e1_exp new_env new_sto


f1 = Lambda "x" (Plus (Var "x") (Var "x"))
f3 = Lambda "f" (App (Var "f") (Num 10))
e3 = App f3 $ f1

showValueInt :: Value -> Int
showValueInt v = case v of
    NumV i -> i
    _      -> error"Not Int"

showResult a = showValueInt $ fst a