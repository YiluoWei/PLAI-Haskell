module Chapter5 where
import Data.Maybe

data Funcdef = Function String String Expression

data Expression =
    NumE Integer |
    IdentifierE String |
    AppE String Expression |
    PlusE Expression Expression |
    MultE Expression Expression

interpretExpWithFunc :: Expression -> [Funcdef] -> Integer
interpretExpWithFunc exp fs = case exp of
    NumE i -> i
    PlusE e1 e2 -> (interpretExpWithFunc e1 fs) + (interpretExpWithFunc e2 fs)
    MultE e1 e2 -> (interpretExpWithFunc e1 fs) * (interpretExpWithFunc e2 fs)
    AppE func e -> let mfuncdef = getFuncdef func fs in case mfuncdef of
                    Nothing -> error"No definition for function"
                    Just (Function name symbol expression) -> interpretExpWithFunc (substitution e symbol expression) fs
    IdentifierE i -> error"Not binded variable"

getFuncdef :: String -> [Funcdef] -> Maybe Funcdef
getFuncdef func fs = foldl (\x y -> if matchFunc func y then Just y else x) Nothing  fs
    where
           matchFunc n1 (Function n2 id exp) = n1 == n2

substitution :: Expression -> String -> Expression -> Expression
substitution what for wherre = case wherre of
    IdentifierE i -> if for == i then what else IdentifierE i
    NumE int -> wherre
    PlusE e1 e2 -> PlusE (substitution what for e1) (substitution what for e2)
    MultE e1 e2 -> MultE (substitution what for e1) (substitution what for e2)
    AppE func e -> AppE func (substitution what for e)
