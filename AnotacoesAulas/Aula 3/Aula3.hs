module Aula3 where

-- CAP 3

-- Void nesse representa 0 (nenhum valor)
data Void

-- Sozinho nesse caso tem 1 valor
data Sozinho = Sozinho deriving Show -- tb como ()

-- Bool Seria 2
-- data Bool = True | False deriving (Show, Eq...)
-- Tipo de enumeacao dia com 7 valores
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
  deriving (Eq, Show, Ord, Enum, Read)
-- Eq, da poderes de comparacao
-- Show, para imprimir em tela
-- Ord, q eles tem ordem
-- Enum, sao enum
-- read, conversao de string por exemplo

-- funcao parcial. So trata 7 casos
converterDia :: String -> Dia -- exemplo do read
converterDia x = read x

-- funcao convDia eh total
convDia :: String -> Either String Dia
convDia "Segunda" = Right Segunda
convDia "Terca" = Right Terca
convDia "Quarta" = Right Quarta
convDia "Quinta" = Right Quinta
convDia "Sexta" = Right Sexta
convDia "Sabado" = Right Sabado
convDia "Domingo" = Right Domingo
convDia _ = Left "Dia invalido"

-- Either e Maybe sao usados para controlar erros sem controle de excecoes.
safeHead :: String -> Maybe Char -- Verifique usando a funcao head
safeHead "" = Nothing
safeHead xs = Just (head xs)

-- PATTERN MATCHING: HABILITA PODERMOS DESCONTRUIR UM TIPO NA ENTRADA DE UMA FUNCAO (DENTRO DE UM CASE E let TB)
-- cOM ISSO, PODE-SE ENXERGAR UM VALOR NA ENTRADA E USAR ISTO PAR ADAR A FUNCAO DIFERENTES COMPORTAMENTOS.

-- Funcao total pois todos os casos dentro do dia foram tratados
-- O caso default pode ser representado por uma variavel usa-se underscore quando nao se usa nada
agenda :: Dia -> String
agenda Terca = "Dia de Haskell..."
agenda Quarta = "Dia de futebolzinho..."
agenda Quinta = "Dia da pizza"
agenda Sexta = "Dia da maldade"
agenda Sabado = "Dia de estudar"
agenda _ = "Dia da tristeza"

-- toDia eh chamada de funcao parcial
-- Uma funcao que a casos n tratados no pattern matching
-- Com o Either a funcao toDia virou total.
toDia :: Int -> Either String Dia -- Ou devolve string ou devolve dia
toDia 1 = Right Domingo
toDia 2 = Right Segunda
toDia 3 = Right Terca
toDia 4 = Right Quarta
toDia 5 = Right Quinta
toDia 6 = Right Sexta
toDia 7 = Right Sabado
toDia _ = Left "Dia invalido"

-- Aluno Como se fosse classe (algebraic data types)
-- Aluno nesse caso tem:
-- String*Int + String
-- Either (String,Int) String
data Aluno = Aluno String Int | Especial String
  deriving (Eq, Show)

fazerNiver :: Aluno -> Aluno
fazerNiver (Aluno nome idade) = Aluno nome (idade+1)
fazerNiver x = x

addSobrenome :: String -> Aluno -> Aluno
addSobrenome s (Aluno nome idade) = Aluno (nome ++ " " ++ s) idade
addSobrenome s (Especial nome) = Especial (nome ++ " " ++ s)

data Correncia = Real | Euro | Dollar deriving (Eq, Show)

-- Double*3 (Double*Correncia)
data Dinheiro = Dinheiro Double Correncia deriving Show

convDol :: Dinheiro -> Dinheiro
convDol (Dinheiro v Real) = Dinheiro (0.19*v) Dollar
convDol (Dinheiro v Euro) = Dinheiro (1.19*v) Dollar
convDol x = x

-- Outras formas de fazer o mesmo
-- RECORD SYNTAX: eh o ato de nomear os campos. Esse nomes sao funcoes de projecao (getters)
data Dinheiro2 = Dinheiro2 {
  valor :: Double,
  correncia :: Correncia
} deriving Show

-- GUARDS:  Checam condicoes booleans em ordem (if encadeado).
convDol' :: Dinheiro2 -> Dinheiro2
convDol' v 
    | correncia v == Real = Dinheiro2 (0.19*valor v) Dollar
    | correncia v == Euro = Dinheiro2 (1.19*valor v) Dollar
    | otherwise = v

modulo :: Double -> Double
modulo x
      | x >= 0 = x
      | otherwise = -x

