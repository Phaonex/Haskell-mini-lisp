module Main (main) where

import Lib

type Ident = String

data Expr   = Number Int
            | Boolean Bool
            | Plus Expr Expr
            | Minus Expr Expr
            | If Expr Expr Expr
            | Equals Expr Expr
            | Var Ident
            | Let Defn Expr
            | Lam [Ident] Expr
            | Apply Expr [Expr]
            deriving (Show, Eq)

data Value  = NumbVal Int | BoolVal Bool | Closure [Ident] Expr Env
            deriving (Show, Eq)

data Defn = Val Ident Expr
          | Rec Ident Expr
          deriving (Show, Eq)

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Number i)     env = NumbVal i
eval (Boolean b)    env = BoolVal b
eval (Equals e1 e2) env = BoolVal $ (eval e1 env) == (eval e2 env)    
eval (Plus e1 e2)   env = let (NumbVal n1) = eval  e1 env in
                          let (NumbVal n2) = eval  e2 env in
                          NumbVal (n1 + n2)

eval (Minus e1 e2)  env = let (NumbVal n1) = eval  e1 env in
                          let (NumbVal n2) = eval  e2 env in
                          NumbVal (n1 - n2)

eval (Var i)        env = find env i
eval (Let d e)      env = eval e (elab env d)
eval (Lam ids e)    env = Closure ids e env
eval (If g e1 e2)   env = case eval g env of
                    (BoolVal True) -> eval e1 env
                    (BoolVal False) -> eval e2 env
eval (Apply f xs) env  = apply f' xs'
    where f' = eval f env
          xs'  = map (flip eval env) xs

apply :: Value -> [Value] -> Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _    = error "using a value as a function!"
find :: Eq a => [(a, b)] -> a -> b
find env i = snd $ head $ filter (\(i', _) -> i == i') env

elab env (Val i e) = (i, eval e env):env
elab env (Rec i (Lam args e)) = env' where env' = (i, Closure args e env'):env
elab _  _                     = error "Expected recursive Lambda"

main :: IO ()
main = putStrLn "Hello, Haskell!"
e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)])))))
ex = e (Apply (Var "sum")[Number 3])