module Main where 

-- (>>=) :: m a -> (a -> m b) -> m b

main :: IO () 
main = do 
       putStrLn "Digite um numero: " 
    >> readLn 
    >>= \x -> putStrLn "Digite outro numero: "
    >> readLn
    >>= \y -> putStrLn ("A soma eh: " ++ show (x+y))

main'' :: IO () 
main'' = do 
    putStrLn "Digite um numero: "
    x <- readLn
    putStrLn "Digite outro numero: "
    y <- readLn
    putStrLn ("A soma eh: " ++ show (x+y))
    
main' :: IO () 
main' = do 
    putStrLn "Digite um nome: "
    nome <- getLine 
    putStrLn ("Seu nome eh: " ++ nome)
