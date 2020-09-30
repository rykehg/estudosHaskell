module Aula6 where

import Data.Monoid
-- Polimorfismo Parametrico: É quando sua função/tipo possui variáveis de tipo.
-- É usado para ter, em tempo de execução, varias ocorrencias da mesma função (tipo)
-- sem mudar o código ou sem criar novos nomes.

-- (,) => Multiplicação
-- Either => Soma
-- (->) => Potencia
-- a -> b => a ^ b
-- data () = () -- 1 VALORES
-- data Bool = True | False -- 2 VALORES

-- Funcao para testar o oof --> (oof . bin)
bin :: Bool -> Int
bin False = 0
bin True = 1

-- foo :: a*a -> a^2
foo :: (a,a) -> (Bool -> a) -- Parenteses a direita podem ser ignorados
foo (x,y) False = x
foo (x,y) True = y

-- oof :: a^2 -> a*a
oof :: (Bool -> a) -> (a,a)
oof f = (f False, f True)

-- foo . oof = id
-- PROP (foo . oof) f = f
-- DEM (TESTE de MESA - RACIOCINIO POR EQUACOES)
--    foo (oof f) =
--    foo (f False, f True)
--    1) SE CALCULARMOS foo (f False, f True) EM False:
--    foo (f False, f True) False = 
--    f False
--    2) SE CALCULARMOS foo (f False, f True) EM True:
--    f True

-- oof . foo = id
-- PROPO: (oof . foo) (a,b) = (a,b)
-- DEM (TESTE DE MESA):
--    oof (foo (a,b)) = 
--    (foo (a,b) False, foo (a,b) True)
--    (a,b)

-- Logo, (a,a) e (Bool -> a) SAO O MESMO TIPO.
------------------------------
-- DESAFIO 1) Prove que 1+1 = 2

-- DESAFIO 2) Prova que a+a = 2*a

-- Tipos polimorficos: É quando um tipo possui variáveis de tipo.
-- Tipos ja vistos
data Curso1 = ADS1 | SI1 | GE1 | GP1 | LOG1
  deriving Show

data Aluno = Aluno String Int
  deriving Show

data Cliente = Especial String Double 
              | Convidado String
  deriving Show

-- O tipo polimorfico
data Bolsa a = Bolsa a a a deriving Show

data Mochila a b = Mochila a a b deriving Show

-- Typeclasses: São restrições a funcoes/tipos polimórficos.
-- Um tipo vai prover funções a serem implementadas de acordo com o tipo desejado. 
-- show :: Show a => a -> String
-- O typelcass show vai prover implementações para cada a  de acordo com a necessidade.
-- O deriving prove essas implementações de forma automática.
data Curso = ADS | SI | GE | GP | LOG
  deriving Eq -- Não customisar igualdade deixe o haskell fazer

class Enumerar a where
  enumerar :: a -> Int

-- Semelhante ao que o Haskell faz com o "deriving Show"
instance Show Curso where
  show ADS = "Curso de Cobol"
  show SI = "Curso de Desing"
  show GE = "Curso de Emo"
  show GP = "Curso de Navio"
  show LOG = "Curso de Caminhao"

instance Enumerar Curso where
    enumerar ADS = 100
    enumerar SI = 101
    enumerar GE = 102
    enumerar GP = 103
    enumerar LOG = 104

instance Enumerar Bool where 
    enumerar False = 0 
    enumerar True = 1

instance Enumerar [a] where 
    enumerar xs = length xs

data Bolsa2 a = Bolsa2 a a 
    deriving Show
-- Fazendo um Eq para bolsa sem usar deriving
instance Eq a => Eq (Bolsa2 a) where
    Bolsa2 c1 c2 == Bolsa2 c3 c4 =
           ((c1 == c3) && (c2 == c4))
        || ((c1 == c4) && (c2 == c3))

-- Monoide
data Xor = Xor Bool deriving Show

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

-- No livro, mappend
--  mappend (Xor a) (Xor b) = 
    -- Xor (xor a b)
instance Semigroup Xor where
  Xor a <> Xor b = Xor (xor a b)

instance Monoid Xor where
  mempty = Xor False -- mempty elemento neutro (propriedade do haskell)
-- Sempre devemos ter um elemento neutro quando montar um monoid, como o 0 da soma ou 1 da multiplicação
-- <> ou mappend - operador do monoid