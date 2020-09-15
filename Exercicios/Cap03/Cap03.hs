module Cap03 where

{- 3.1. Crie o tipo Pergunta com os values constructors Sim ou Nao.
Faça as funções seguintes, determinando seus tipos explicitamente.
 - pergNum : recebe via parâmetro uma Pergunta. Retorna 0 para Nao e 1 para Sim.
 - listPergs : recebe via parâmetro uma lista de Perguntas , e retorna 0 s e 1 s correspondentes aos constructor contidos na lista.
 - and' : recebe duas Perguntas como parâmetro e retorna a tabela verdade do and lógico, usando Sim como verdadeiro e Nao como falso.
 - or' : idem ao anterior, porém deve ser usado o ou lógico.
 - not' : idem aos anteriores, porém usando o not lógico.-}
data Pergunta = Sim | Nao deriving (Show, Eq, Ord, Enum, Read)

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs x = [pergNum y | y <- x]

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' Nao Nao = Nao
and' Nao Sim = Nao
and' Sim Nao = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Sim Sim = Sim
or' Nao Nao = Nao
or' Nao Sim = Sim
or' Sim Nao = Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

{- 3.2. Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kelvin.
Implemente as funções:
 - converterCelsius : recebe um valor double e uma temperatura, e faz a conversão para Celsius.
 - converterKelvin : recebe um valor double e uma temperatura, e faz a conversão para Kelvin.
 - converterFarenheit : recebe um valor double  e uma temperatura, e faz a conversão para Farenheit. -}
data Temperatura = Celsius | Farenheit | Kelvin deriving (Show, Eq, Ord, Enum, Read)

convCelsius :: Double -> Temperatura -> Double
convCelsius x Farenheit = (x - 32) * 5 / 9
convCelsius x Kelvin = x - 273.15
convCelsius x _ = x

convKelvin :: Double -> Temperatura -> Double
convKelvin x Farenheit = (x - 32) * 5/9 + 273.15
convKelvin x Celsius = x + 273.15
convKelvin x _ = x

convFarenheit :: Double -> Temperatura -> Double
convFarenheit x Kelvin = (x - 273.15) * 9/5 + 32
convFarenheit x Celsius = (x * 9/5) + 32
convFarenheit x _ = x

{- 3.3. Implemente uma função que simule o vencedor de uma partida de pedra, papel e tesoura
usando tipos criados. Casos de empate devem ser considerados em seu tipo. -}
data Jokenpo = Pedra | Papel | Tesoura deriving (Show, Eq, Ord, Enum, Read)

jogoJokenpo :: Jokenpo -> Jokenpo -> Either String Jokenpo
jogoJokenpo Pedra Papel = Right Papel
jogoJokenpo Papel Pedra = Right Papel
jogoJokenpo Papel Tesoura = Right Tesoura
jogoJokenpo Tesoura Papel = Right Tesoura
jogoJokenpo Tesoura Pedra = Right Pedra
jogoJokenpo Pedra Tesoura = Right Pedra
jogoJokenpo _ __ = Left "Empate"

{- 3.4. Faça uma função que retorne uma string, com todas as vogais maiúsculas e 
minúsculas eliminadas de uma string passada por parâmetro usando list compreenshion. -}
eliminaVogal :: String -> String
eliminaVogal palavra = [x | x <- palavra, x /= 'a', x /= 'e', x /= 'i', x /= 'o', x /= 'u', x /= 'A', x /= 'E', x /= 'I', x /= 'O', x /= 'U']

{- 3.5. Sabe-se que as unidades imperiais de comprimento podem ser Inch, Yard ou Foot
(há outras ignoradas aqui). Sabe-se que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048. 
Faça a função converterMetros que recebe a unidade imperial e o valor
correspondente nesta unidade. Esta função deve retornar o valor em metros.
Implemente também a função  converterImperial , que recebe um valor em metros 
e a unidade de conversão. Esta função deve retornar o valor convertido para a 
unidade desejada.-}
data UnidadeImperial = Inch | Yard | Foot

converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch x = x * 0.0254
converterMetros Yard x = x * 0.9144
converterMetros Foot x = x * 0.3048

converterImperial :: Double -> UnidadeImperial -> Double
converterImperial x Inch = x / 0.0254
converterImperial x Yard = x / 0.9144
converterImperial x Foot = x / 0.3048

{- 3.6. Faça um novo tipo chamado  Mes, que possui como valores todos os meses 
do ano. Implemente:
 - A função checaFim, que retorna o número de dias que cada mês possui 
 (considere fevereiro tendo 28 dias).
 - A função prox, que recebe um mês atual e retorna o próximo mês.
 - A função estacao, que retorna a estação do ano de acordo com o mês 
 e com o hemisfério.
Use apenas tipos criados pela palavra data aqui. -}
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving Show
data Hemisferio = Norte | Sul
data Estacao = Inverno | Primavera | Verao | Outono deriving Show

checaFim :: Mes -> Int
checaFim Janeiro = 31
checaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

estacao :: Mes -> Hemisferio -> Estacao
estacao Dezembro Norte = Inverno
estacao Janeiro Norte = Inverno
estacao Fevereiro Norte = Inverno
estacao Marco Norte = Inverno
estacao Abril Norte = Primavera
estacao Maio Norte = Primavera
estacao Junho Norte = Primavera
estacao Julho Norte = Verao
estacao Agosto Norte = Verao
estacao Setembro Norte = Verao
estacao Outubro Norte = Outono
estacao Novembro Norte = Outono

estacao Dezembro Sul = Verao
estacao Janeiro Sul = Verao
estacao Fevereiro Sul = Verao
estacao Marco Sul = Verao
estacao Abril Sul = Outono
estacao Maio Sul = Outono
estacao Junho Sul = Outono
estacao Julho Sul = Inverno
estacao Agosto Sul = Inverno
estacao Setembro Sul = Inverno
estacao Outubro Sul = Primavera
estacao Novembro Sul = Primavera

{- 3.7. Faça uma função que receba uma String e retorne
True se esta for um palíndromo; caso contrário, False. -}
ehPalindromo :: String -> Bool
ehPalindromo str = str == reverse str

{- 3.8. Faça uma função que elimine todos os números pares, todos os ímpares múltiplos 
de 7 e negativos de uma lista de inteiros passada via parâmetro. Você deve retornar 
esta lista em ordem reversa em comparação a do parâmetro. -}
ex8 :: [Int] -> [Int]
ex8 x = reverse[n | n <- x, (mod) n 2 /= 0 && (mod) n 7 /= 0, n > 0]

{- 3.9. Faça uma função que recebe três Strings x , y e z como
parâmetro. A função retorna uma tupla com três coordenadas
contendo a ordem reversa em cada. A primeira coordenada deve
conter string reversa do primeiro parâmetro, e assim por diante. -}
ex9 :: String -> String -> String -> (String, String, String)
ex9 x y z = (reverse x, reverse y, reverse z)

{- 3.10. Faça uma função chamada  revNum , que receba uma
String s e um Int n . Esta deverá retornar as n primeiras letras
em ordem reversa e o restante em sua ordem normal. Exemplo:
revNum 4 "FATEC" = "ETAFC"-}

{- 3.11. Crie o tipo de dado Binario que pode ser Zero ou Um. Faça outro tipo de dado 
chamado Funcao que pode ser Soma2, Maior, Menor ou Mult2. Implemente a função 
aplicar que recebe uma Funcao e dois Binarios. Seu retorno consiste em executar 
a operação desejada. Exemplo:
aplicar Soma2 Um Um = Zero -}
aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Um Um = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Zero = Um

aplicar Maior Um Zero = Um
aplicar Maior Zero Um = Zero
aplicar Maior Zero Zero = Zero
aplicar Maior Um Um = Zero

aplicar Menor Um Zero = Zero
aplicar Menor Zero Um = Um
aplicar Menor Zero Zero = Zero
aplicar Menor Um Um = Zero

aplicar Mult2 Um Um = Um
aplicar Mult2 Zero Zero = Zero
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Um Zero = Zero

-- ex3.12
convBinarioDecimal :: Binario -> Int
convBinarioDecimal Um = 1
convBinarioDecimal Zero = 0

binList :: [Binario] -> [Int]
binList str = [convBinarioDecimal(aplicar Soma2 n Um) | n <- str]

{- 3.12. Faça uma função chamada binList , usando list compreeshion, 
que recebe uma lista de Binarios (ver exercício anterior) e retorna 
outra lista com elemento somado Um e convertido para Int. Exemplo:
binList [Um, Zero, Zero, Um, Zero] = [0,1,1,0,1] -}
data Binario = Zero | Um deriving (Eq, Show)

somaBin :: Binario -> Int
somaBin Zero = 1
somaBin Um = 0

binList :: [Binario] -> [Int]
binList x = [somaBin y | y <- x]

{- 3.13. Faça um novo tipo chamado  Metros , que possui um \textit{value constructor}
de mesmo nome, cujos parâmetros são: um  Int que representa a dimensão, e um Double que 
representa o valor da medida e outro chamado MetragemInvalida . Implemente as funções:
 - areaQuadrado :: Metros -> Metros : calcula a área de um quadrado.
 - areaRet :: Metros -> Metros -> Metros : calcula a área de um retângulo.
 - areaCubo :: Metros -> Metros : calcula a área de um cubo.
Exemplo:
Prelude> areaQuadrado (Metros 1 2.0) 
Metros 2 4.0
Use o pattern matching para ignorar as metragens erradas (calcular a área de um quadrado 
com um lado de dimensão 4 não é válido). -}

{- 3.14. Faça o novo tipo  Valido  que possui dois value constructors Sim e Nao . O value
constructor  Sim possui um parâmetro (campo)  String . Implemente uma função isNomeValido 
que recebe um nome e retorna  Nao caso a String seja vazia; caso contrário, Sim . -}

{- 3.15. Refaça o exercício 3 do capítulo anterior usando record syntax e tipos com 
parâmetro (siga o exemplo da conversão de medidas SI para imperial). -}

{- 3.16. Faça o tipo Numero , que possui um value constructor Ok com um campo double 
e outro value constructor com um campo String . Faça a função dividir que divida dois
números e, caso o segundo número seja 0, emita um erro (use o pattern matching). Exemplo:
Prelude> dividir (Numero 6) (Numero 5) 
Numero 1.2. Erro -}

{- 3.17. Faça o tipo Cripto que possua dois values constructors Mensagem e Cifrado,
ambos com um campo String e um value constructor Erro. Faça as funções 
encriptar e decriptar, seguindo cada exemplo a seguir.
Prelude> encriptar (Mensagem "FATEC") 
Cifrado "GBUFD" 
Prelude> decriptar (Cifrado "DBTB") 
Mensagem "CASA"
Veja que a encriptação deve empurrar cada letra a frente e a
decriptação faz o inverso, empurrando uma letra para trás. Use as
funções succ e pred, e também list compreeshions. Não é
possível encriptar mensagens cifradas e decriptar mensagens. -}
data Cripto = Mensagem String | Cifrado String | Erro deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem x) = Cifrado [ succ y | y <- x]
encriptar (Cifrado z) = Erro
encriptar _ = Erro

decriptar :: Cripto -> Cripto
decriptar (Cifrado x) = Mensagem [ pred y | y <- x]
decriptar (Mensagem z) = Erro
decriptar _ = Erro

{- 3.18. Faça uma função encriptarTodos que encripta (ou dá
erro) todos os elementos de um vetor de Cripto -}
encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos cptList = [ encriptar x | x <- cptList]

{- 3.19. Tendo como base o exercício de conversão de medidas, crie uma função que 
faça conversão de câmbio. Você deve criar o tipo Cambio contendo os value 
constructors Euro , Real e  Dollar. Crie também o tipo Moeda que possui 
os campos val :: Double e cur :: Cambio. Use record syntax e as taxas de
conversão do dia no qual você fez o exercício. -}
data Cambio = Euro | Real | Dollar deriving Show
data Moeda = Moeda {val :: Double, cur :: Cambio} deriving Show

converteMoedas :: Moeda -> Cambio -> Moeda
converteMoedas (Moeda x Real) Euro = Moeda (x * 0.16) Euro
converteMoedas (Moeda x Dollar) Euro = Moeda (x * 0.91) Euro
converteMoedas (Moeda x Dollar) Real = Moeda (x * 5.73) Real
converteMoedas (Moeda x Euro) Real = Moeda (x * 6.28) Real
converteMoedas (Moeda x Real) Dollar = Moeda (x * 0.17) Dollar
converteMoedas (Moeda x Euro) Dollar = Moeda (x * 1.09) Dollar
converteMoedas (Moeda x y) z = Moeda x z

{- 3.20. Crie a função converterTodosReal que recebe uma lista de moedas 
e retorna outra lista de moedas com todos os seus
elementos convertidos para Real. Use list compreenshion. -}
converterTodosReal :: [Moeda] -> [Moeda]
converterTodosReal mdList = [ converteMoedas md Real | md <- mdList ]

{- 3.21 Crie a função maxMoeda que recebe uma lista de moedas
e retorna o valor máximo absoluto (sem conversão alguma) dentre
os campos val desta lista. Exemplo:
Prelude > maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro] 7
Use a função maximum -}
maxMoeda :: [Moeda] -> Double
maxMoeda mdList = maximum [ val md | md <- mdList ]
