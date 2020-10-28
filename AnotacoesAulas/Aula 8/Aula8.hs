module Aula8 where

tamanho :: String -> Int
tamanho xs = lenght xs

-- Troca-se o funtor [] pelo funtor maybe
-- safeHead :: [] a -> Maybe a
safeHead :: [a] Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

data Id a = Id a deriving Show

-- fmap id x = x

-- fmap id (Id x) = 
-- (Id (id x)) =
-- Id x OK

-- fmap (g.h) (Id x) =
-- id ((g.h) x) = 
-- Id (g ( h x)) =
-- fmap g (Id (h x)) =
-- fmap g (fmap h (Id x)) =
-- (fmap g . fmap h) (Id x) OK

instance Functor Id where
  fmap f (Id x) = Id (f x)

--f = id, g = []
-- Troca-se Funtor Id pelo Funtor []
-- return para listas (m =[])
toList :: Id a -> [] a 
toList (Id x) = [x]

{- TRANSFORMACAO NATURAL: um TN eta :: f ~> g, onde 
f e g são funtores, é a familia eta_a :: f a -> g a 
que satisfaz a seguinte lei: 
Para todo h :: a -> b 
fmap_g h . eta_a = fmap_f h -}

{-
return TROCA Id por m
Join TROCA m . m POR m

class Functor m => Monada m where
  return :: a -> m a
  join :: m (m a) -> m a

instance Monada [] where
  return x = [x]
  join = concat

instance Monada maybe where
  return x = Just x
  join (Just (Just x)) = Just x
  join _ = Nothing

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> mb) -> m b

instance Monad Maybe where 
    return x = Just x
    (Just x) >>= f = f x
    Nothing >>= f = Nothing
-}

data Matriz a = Matriz [[a]] deriving Show

instance Functor Matriz where
  fmap f (Matriz ms) = Matriz (map (map f) ms)

-- f = [] [], g = []
join :: Matriz a -> [] a
join (Matriz ms) = concat ms

par :: Int -> Bool
par n = even n

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) x f = join (fmap f x)

infixl 9 |> --p poder fazer operador ignora todos os outros operador (ordem de prioridade 9 - max)

{-
4 |> \x -> 2*x+1 |> \y -> x+y = 
4 |> \x -> (2*x+1 |> \y -> x+y) = 
(\x -> (2*x+1 |> \y -> x+y)) 4 = 
(\x -> (\y -> x+y) (2*x+1)) 4 =
(\y -> 4+y) 9 =
4+9 =
13

x = 4
y = 2*x+1
return y
-}

-- Just 4 >>= \x -> Just (2*x+1) >>= \y -> Just (x+y)   
foo :: Int -> Maybe Int 
foo z = do
   x <- Just z  
   y <- Just (2*x+1)  
   return (x+y)