module Aula5 where

{- Currying: Permite criar novas funcoes a partir de conhecidas suprimindo a quantidade
de parametros. Se tivermos uma funcao com n parametros, podemos criar novas 
funcoes passando: n-1, n-2, ..., 2, 1 argumento(s).-}

somar :: Int -> Int -> Int -> Int
somar x y z = x + y + z

{- High-order functions: Sao funcoes que recebem e/ou retornam outras funcoes.
Em Haskell, as funcoes se comportam como valores. A ordem dos paranteses importam.
Int -> (Int -> Int) = Int -> Int -> Int ==> 2 entradas Int
/=
(Int -> Int) -> Int ==> 1 estrada funcao
O parenteses a esquerda é como se fosse uma coisa só -}

dobro :: Int -> Int
dobro x = 2 * x

foo :: (Int -> Int) -> Int
foo f = 1 + f 5

data Oper = Soma | Mult | Sub | Div
  deriving Show

oper ::  Oper -> (Int -> Int -> Int)
oper Soma = \x y -> x+y
oper Sub = (-)
oper Mult = (*)
oper Div = \x y -> div x y

tamanho :: String -> Int
tamanho xs = length xs

ehPar :: Int -> Bool
ehPar x = even x

bin :: Bool -> Int
bin False = 0
bin True = 1

contaPar :: Int -> Int -> Int
contaPar cont 1 
        | even 1 = cont + 1
        | otherwise = cont

{- map :: (a -> b) -> [a] -> [b] recebe uma funcao (a-> b) e distribui ela para 
TODOS os elementos da lista [a] -}

{- filter :: (a -> Bool) -> [a] -> [a] Recebe um predicado (a -> Bool) e uma lista
[a], retorna os elementos que retornarem True do predicado -}

-- Fold: Ex do pai do tail recursion optimization - destroi uma lista
fold :: (b -> a -> b) -> b -> [a] -> b
fold _ acc []     = acc
fold f acc (x:xs) = fold f (f acc x) xs

-- Mais parecido com o memoization - constroi uma lista
unfold :: (b -> Maybe (a,b)) -> b -> [a]
unfold f x = 
    case f x of
      Just (a,b) -> a : unfold f b
      Nothing -> []

naturais :: Int -> Maybe (Int,Int)
naturais n
  | n <= 5 = Just (n,n+1)
  | otherwise = Nothing

-- Fazer o fibonacci usando o unfold.