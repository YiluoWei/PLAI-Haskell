module Chapter6 where

import Data.Maybe

data Funcdef = Function String String Expression

data Expression =
    Num Int |
    Identifier String |
    App String Expression |
    Plus Expression Expression |
    Mult Expression Expression

getFuncdef :: String -> [Funcdef] -> Funcdef
getFuncdef func fs = foldl (\x y -> if matchFunc func y then y else x) (error"undefined func") fs
    where
           matchFunc n1 (Function n2 id exp) = n1 == n2

data Bind = Binding String Int

type Environment = [Bind]

interpretWithEnv :: Expression -> Environment -> [Funcdef] -> Int
interpretWithEnv exp env fds = case exp of
    Num i -> i
    Plus e1 e2 -> interpretWithEnv e1 env fds + (interpretWithEnv e2 env fds)
    Mult e1 e2 -> interpretWithEnv e1 env fds * (interpretWithEnv e2 env fds)
    Identifier i -> lookupEnv i env
    App funcName e0 -> let (Function n i e) = getFuncdef funcName fds in
                       let newenv = [(Binding i (interpretWithEnv e0 env fds))] in
                       interpretWithEnv e newenv fds

lookupEnv :: String -> Environment -> Int
lookupEnv s0 [] = error"Not found in environment"
lookupEnv s0 ((Binding s1 number):bs) = if s0 == s1 then number else lookupEnv s0 bs


