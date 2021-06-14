import Data.Char
{------------------------------------------ 
Prova 01 Programação Funcinonal 25/08/2020
-------------------------------------------
Nome completo 01: Vitor Risso Parisi
Matricula 01: 2016.1.08.037

-------------------------------------------
Instruções para o preenchimento do script
a) Implemente o código na sequência em que aparece na prova
b) separe cada questão com as linhas pontilhadas (abaixo)
   {--Questão X -----------------------------------------------}
c) Questões com mais de um item, separe-os da seguinte forma:

{--Questão X ----------------------------------------------}

--Item X.a
 código da questão X item a

--Item X.b
  código da questão 
  
d) Use sempre os nomes das funções indicados na prova.
Caso o nome não seja sugerido, dê preferência por começar como: funcQx, em que x refere-se à questão.
Ex: funcQ3a pode ser a implementação da questão 3, item a

e) não é necessário (mas não é um problema) incluir o enunciado das questões no script
f) Você pode inserir comentários pessoais explicando o código implementado antes do cabeçalho da função

--Questão 1 -----------------------------------------------}
--Item 1.a
func1a:: Float->Float
func1a x
  |x >= 0     = (x+4)/(x+2)
  |otherwise  = x/2  
--Item 1.b
func1b :: Int -> Int -> Int
func1b x y
  |x >= y     = x+y
  |otherwise  = x-y

--Item 1.c
func1c :: Int -> Int -> Int -> Int
func1c x y z
  |(x+y) > z  = x+y+z
  |(x+y) < z  = x-y-z
  |otherwise  = 0

--Questão 2 -----------------------------------------------}
fat :: Int->Int
fat 1 = 1
fat x = x *fat(x-1)

fat1 :: Int -> Int
fat1 x
  | x==1     = 1
  |otherwise = x *fat(x-1)

--Questão 3 -----------------------------------------------}
soma :: Int -> Int -> Int
soma x y =  x+y

mult :: Int -> Int -> Int
mult x y
  |y == 1     = x
  |otherwise  = soma x (mult x (y-1))

--Questão 4 -----------------------------------------------}
invertInt :: Int -> Int
invertInt x = read (reverse (show x)) :: Int
-- show 123 =  converte string para Int
-- reverse "123" = inverte a string "321"
-- read "321" :: Int = convert string para Int

--Questão 5 -----------------------------------------------}
square :: Int -> Int 
square x = x * x

fourPower :: Int -> Int
fourPower x = (square x) * (square x)

--Questão 6 -----------------------------------------------}
sequencia6 :: Int -> Float
sequencia6 x
  |x==0      = sqrt 6
  |otherwise = sqrt (6 + sequencia6(x-1)) 


--Questão 7 -----------------------------------------------}
fatF :: Float->Float
fatF 1 = 1
fatF x = x *fatF(x-1)

funcQ7 :: Float -> Float -> Float
funcQ7 n m
  |m >= n      = (fatF m)/ ((fatF n) * (fatF(m-n)))
  |otherwise  = -1


--Questão 8 -----------------------------------------------}
funcQ8 :: Int -> Int -> Int
funcQ8 m n
  | m == 0  = n
  | m > 0   = funcQ8(n `mod` m) m
  | m > n   = funcQ8(m `mod` n) n
  | otherwise = -1

--Questão 9 -----------------------------------------------}
funcQ9 :: Int -> Int -> Int -> Int
funcQ9 n i f
  | n > f = 0
  | i > f = 0
  | i`mod`n == 0 = (funcQ9 n (i+1) f) + 1
  | otherwise = (funcQ9 n (i+1) f) + 0 

--Questão 10 -----------------------------------------------}
--roundN :: Float -> Int
--roundN x = round ((read (reverse (show x))*10))

--funcQ10 :: Float -> Int
--funcQ10 x
--  | (roundN n) > 5 = (roundN n)-1
--  | otherwise      = roundN n

funcQ10 :: Int -> Int 
funcQ10 x = read ((last(show x)) : []) :: Int 


--Questão 11 -----------------------------------------------}
funcQ11 :: Int -> Int -> Int
funcQ11 p n
  | (length (show(n))) <= p = -1
  | otherwise               = read (((show n) !! p):[]) :: Int

--Questão 12 -----------------------------------------------}
-- R: O codigo esta errado, pois ela nao ve se todos numero sao diferentes entre si, ele nao compara se m=/p ! 
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)


--Questão 13 -----------------------------------------------}
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual m n p 
  |(m==n) && (n==p) = 3
  |(m==n) ||  (n==p) ||  (n==p) = 2
  |otherwise = 0


--Questão 14 -----------------------------------------------}
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

--Item 14.a
howManyLess :: Int -> Int -> Int
howManyLess _(-1) = (0)
howManyLess v p 
  |(vendas p) < v = (howManyLess v (p-1)) +1
  |otherwise = (howManyLess v (p-1))+0

--Item 14.b
noZeroInPeriod :: Int -> Bool
noZeroInPeriod p
  | p == -1 = False
  | (vendas p) == 0 = ((noZeroInPeriod (p-1)) ||  True)
  | otherwise       = ((noZeroInPeriod (p-1)) ||  False)

--Item 14.c
zerosInPeriod :: Int -> [Int]
zerosInPeriod p
  | p == -1 = []
  | vendas p == 0 =  (zerosInPeriod (p-1))++( p:[])
  | otherwise = zerosInPeriod (p-1)

--Item 14.d
diaVendaMenos :: Int -> Int -> [Int]
diaVendaMenos _(-1) = []
diaVendaMenos v p
  |(vendas p) < v = (diaVendaMenos v (p-1))++(p:[])
  |otherwise = (diaVendaMenos v (p-1))


--Questão 15 -----------------------------------------------}
--round (log(sqrt 5*x+0.9)/log((sqrt 5+1)/2))
-- Como eu nao aprendi a fazer uma atribuicao de uma variavel para outra, tive que sempre passar 2 ints iguais para a funcao antFib x y  funcionar
fib::Int->Int
fib x
  | x == 0 = 0
  | x == 1 = 1
  | x > 1 = fib (x-2) + fib (x-1)

antFib :: Int-> Int -> Int
antFib x y
  |(fib x) == y = x 
  |((fib x) < y && (x<4))= antFib(x+1) y
  |(fib x) >y = antFib(x-1) y
  |otherwise  = -1   

--Questão 16 -----------------------------------------------}
funny :: Int->Int->Int->Bool
funny x y z = ((x > z) && (y <= x)) || ((x < z) && (y <= x))

--Questão 17 -----------------------------------------------}
funcQ17:: Char -> Char
funcQ17 x
  | isUpper x == True = x
  | otherwise = toUpper x

--Questão 18 -----------------------------------------------}
funcQ18::Char -> Int
funcQ18 x
  | isDigit x == False = -1
  | otherwise = (ord x)-48

--Questão 19 -----------------------------------------------}
funcQ19::String -> Int -> String
funcQ19 s n
  |(length s) >= n = s
  |otherwise = '>':(funcQ19 s (n-1))

--Questão 20 -----------------------------------------------}

--Questão 21 -----------------------------------------------}
-- infixl infxr infix

--Questão 22 -----------------------------------------------}
reverso :: [Int] -> [Int]
reverso [] = [] -- base de indução (elemento de parada da induçao)
reverso (a:b) = (reverso b) ++[a] -- indução recursiva 

--Questão 23 -----------------------------------------------}
converte :: [Int] -> String
converte [] = []
converte (a:b) 
  |a <=26  = (chr(a+96)):(converte b)
  |otherwise = (converte b)

--Questão 1..7 -----------------------------------------------}
--1..7 = [1,2,3,4,5,6,7]
--(a) ['a'..'g'] =  "abcdefg"
--(b) [0.1 ..0.9] =  [0.1,1.1]
--(c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.8999999999999999]
--(d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999,1.3,1.5,1.7,1.9]
--(e) [0.4,0.2 ..0.8] =[]
--(f) [1,4..15] = [1,4,7,10,13]


--Questão 24 -----------------------------------------------}
funcQ24::String -> Char -> Int
funcQ24 [] c = 0
funcQ24 (a:b) c
  |(ord a)/=(ord c) = (funcQ24 b c) + 0
  |otherwise = (funcQ24 b c) + 1
--Questão 25 -----------------------------------------------}

--Questão 26 -----------------------------------------------}
multLista :: Int-> Int ->[Int]
multLista x 0 = []
multLista x y = x:(multLista x (y-1))

proliferaInt :: [Int] ->[Int]
proliferaInt [] = []
proliferaInt (a:b) = (multLista a a) ++ (proliferaInt b)


--Questão 27 -----------------------------------------------}
multChar::Char -> Int -> String
multChar x 0 = []
multChar x y = x:(multChar x (y-1))

proliferaChar::String -> String
proliferaChar [] = []
proliferaChar (a:b) = (multChar a ((ord a)-96)) ++ (proliferaChar b)