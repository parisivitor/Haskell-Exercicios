import Data.Char
import Data.List
{------------------------------------------ 
Prova 02 Programação Funcinonal 06/10/2020
-------------------------------------------
Nome completo 01: Vitor Risso Parisi
Matricula 01: 2016.1.08.037

Nome completo 02:
Matrícula 02

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
g) caso alguma função tenha um nome já utilizado em outra questão, use números para diferenciá-las, como: teste, teste2, teste3 ...

-------------------------------------------}

--{--Questão 1 -----------------------------------------------}
--Item 1.a
   
type Brinde = (String,Int, Int) 

mapa :: Int -> (Brinde)
mapa 01 = ("Natal", 21,34)
mapa 02 = ("Bertioga",17,65)
mapa 03 = ("Rio de Janeiro", 9, 10)
mapa 04 = ("Curitiba", 3, 54)
mapa 05 = ("Petrolina", 2 , 09)
mapa 06 = ("Salvador", 0 , 01)
mapa 07 = ("Teresinha", 21, 56)   

--Item 1.b
cidade::Brinde -> String
cidade (a,b,c) = a

nPassagem:: Brinde -> Int
nPassagem (a,b,c) = b

nHospedagens:: Brinde -> Int
nHospedagens (a,b,c) = c

--Item 1.c
totalPassagens :: Int -> Int
totalPassagens 0 = 0
totalPassagens n = (nPassagem (mapa n)) + totalPassagens(n-1)

--Item 1.d
totalHospedagens :: Int -> Int
totalHospedagens 0 = 0
totalHospedagens n = (nHospedagens (mapa n)) + totalHospedagens(n-1)

--Item 1.e
infoCidade:: String -> (Int,Int, Int)
infoCidade s = buscaCidade s 7

buscaCidade:: String -> Int -> (Int,Int,Int)
buscaCidade _ 0 = (0,0,0)
buscaCidade s n
  |s == cidade(mapa n) = (n, nPassagem (mapa n), nHospedagens (mapa n))
  |otherwise = (buscaCidade s (n-1))

--Item 1.f
disponivel:: Brinde -> Bool
disponivel x = comparaTuplas x 7

comparaTuplas:: Brinde -> Int -> Bool
comparaTuplas _ 0 = False
comparaTuplas (s,x,y) n
  |s == cidade( mapa n) && x <= (nPassagem(mapa n)) && y<= (nHospedagens(mapa n)) = True 
  |otherwise = comparaTuplas (s,x,y) (n-1)

--{--Questão 2 -----------------------------------------------}
 --Item 2.a
proximo::[(Char,Int)]->(Char,Int)
proximo [t] = t
proximo ((a,b):(c,d):x)
  |b<d = proximo ((a,b):x)
  |otherwise = proximo ((c,d):x)
-- R: A funcao proximo, compara as tuplas que estao dentro de uma lista, e retorna a tupla que tem o menor valor nos elemento inteiro
--ex: proximo [('s',1),('d',3)] ->  vai retornar a tupla ('s',1)

 --Item 2.b
realizado::(Char,Int)->[(Char,Int)]->[(Char,Int)]
realizado _ [] = []
realizado (k,m) ((a,b):x)
  |m == b = x
  |otherwise = (a,b):(realizado (k,m) x)
--R: A funcao realizado, apos passado 1 tupla, ela excluira a outra tupla de de dentro da lista se as 2 tiverem o mesmo elemento inteiro dentro dela. Se houver outras tuplas com o memso inteiro dentro, apenas a primeira encontrada na recursividade sera excluido e a outra sera mantida dentro da lista. 
--Ex: realizado ('s',3) [('d',3),('x',5),('a',-10)] -> escluira da lista a tupla ('d',3) dando como respsota [('x',5),('a',-10)]
--Ex2:  realizado ('s',3) [('d',3),('x',5),('a',-10),('r',3)] -> excluira a tupla ('d',3) e mantera a outra tupla com o memso valor de iunteiro ('r',3), dando como respsota [('x',5),('a',-10),('r',3)]
--Ex3: realizado ('s',1) [('d',3),('x',5),('a',-10)] -> se nao houver nenhuma tupla com o mesmo valor inteiro, retornara a lista inicial ->  [('d',3),('x',5),('a',-10)]

 --Item 2.c
f01::[(Char,Int)]->String
f01 [] = []
f01 x = (fst (proximo x)):(f01 (realizado (proximo x) x))
--R: Contatena os elementos char, comparando os elementos inteiros do menor para o maior, e se caso houver tuplas com o mesmo valor de inteiro, ela repete o o char da ultima tupla com o valor de inteiro igual, no lugar da outra tupla
--Ex: f01 [('d',3),('x',5),('a',-10),('r',4)] -> concatena os char dentro de uma lista em ordem crescendo dos inteiros tera a saida "adrx"
--Ex: f01 [('d',3),('x',3),('a',-10),('r',3)] -> repetira o r(o ultimo inteiro igual da tupla), todas as vezes que tiver o valor inteiro igual, tendo como saida "arrr"

--Item 2.d 
 --R: a Funcao proximo tem a funcionalidade de comparar os valores inteiros de 2 tuplas, retornando a tupla que contem o menor inteiro, quando a lista for vazia, a funcao proxima nao serve de nada, ja que necessita de 2 ou mais tuplas para que haja uma comparação para saber qual tupla tem o menor valor inteiro. Sendo assim, casa seja passado uma lista vazia, o condigo nao funcionara perfeitamente, exclusivamente para esse caso, em outros casos passando uma lista nao vazia, a rucursividade nunca vai chegar em uma lista vazia sendo que proximo é uma funcao de comparaçao e nao exclusao de tuplas. 

--Item 2.e
--R: t-> tupla de ('Char', Int)
--   a-> é o primeiro elemento(char) da primeira tupla que esta dentro da lista   | caracter       -> Char
--   b-> é o segundo elemento(inteiro) da primeira tupla que esta dentro da lista | numero inteiro -> Int
--   c-> é o primeiro elemento(char) da segunda tuplha que esta dentro da lista   | caracter       -> Char
--   x-> é o resto de tuplas que estao dentro da lista ou a lista vazia           | lista de tuplas-> [(Char,Int)] ou Lista vazia []


--{--Questão 3 -----------------------------------------------}
funcQ3Aux::Int -> Char
funcQ3Aux x = chr (x + 97)

converte::[Int] -> String
converte [] = []
converte (x:y) = (funcQ3Aux x):(converte y)

comparaString::String -> String -> Int
comparaString "" _ = (-1)
comparaString _ [] = 1
comparaString x (a:b)
  |comparaChar x a < 0 = -1
  |otherwise = comparaString x b

comparaChar::String -> Char -> Int
comparaChar [] _ = 1
comparaChar (x:y) a
 |a == x = -1
 |otherwise = comparaChar y a

funcQ3result::[[Int]] -> String -> [String]
funcQ3result [] _ = []
funcQ3result (x:y) z
 |(comparaString (converte x) z) > 0 = funcQ3result y z
 |otherwise = (converte x):(funcQ3result y z)

funcQ3tira::[String]->[String]
funcQ3tira[] = []
funcQ3tira (a:b)
  |a=="" = funcQ3tira b
  |otherwise = (a : funcQ3tira b)

funcQ3::[[Int]] -> String -> [String]
funcQ3 a b = funcQ3tira (funcQ3result a b)

--{--Questão 4 -----------------------------------------------}
funcQ4::[String] -> [(Int,String)]
funcQ4 [] = [] 
funcQ4 z = [(funcQ4conta a,a)| a <-z]

funcQ4conta:: String -> Int
funcQ4conta "" = 0
funcQ4conta (a:b) = funcQ4conta(b) +1

--{--Questão 5 -----------------------------------------------}
--contaposicao::[Int] -> Int -> [Int]
--contaposicao[] _ = []
--contaposicao (a:b) c
--  | c `mod`2 == 0 = a :(contaposicao b (c+1))
--  |otherwise = (contaposicao b (c+1))

--posicao::[([Int],Bool)] -> [[Int]]
--posicao [] = []
--posicao ((a,b):c)
--  |b==False  = (contaposicao a 0) : (posicao c) 
--  |otherwise =  a : ( posicao c)

compara::[Int] -> [Int] -> [Int]
compara _ [] = []
compara (x:y) (a:b)
 |x==0 = a:(compara y b)
 |otherwise = compara y b

funcQ5Aux:: [Int] -> [Int]
funcQ5Aux [] = []
funcQ5Aux x = compara (map (`mod`2) ([0..length x])) x

funcQ5::[([Int],Bool)] -> [[Int]]
funcQ5 [] = []
funcQ5 ((x,y):z)
 |y = x:(funcQ5 z)
 |otherwise = (funcQ5Aux x):(funcQ5 z)

--{--Questão 6-----------------------------------------------}
funcQ6Aux:: [Int] -> Int -> [Int]
funcQ6Aux [] _ = [] 
funcQ6Aux (x:y) z
 |(x `mod` 2) /= 0 = x:(funcQ6Aux y 1)
 |z == 1 = funcQ6Aux y 0
 |otherwise = x:funcQ6Aux y 0

funcQ6:: [Int] -> [Int]
funcQ6 x = funcQ6Aux x 0

--{--Questão 7-----------------------------------------------}
funcQ7c:: [Int] -> [Int] -> Int -> [Int]
funcQ7c [] [] _ = []
funcQ7c (a:b) (c:d) n
  |a == c = (funcQ7c b d (n+1) )
  |otherwise = (n) : (funcQ7c b (c:d) (n+1) )

funcQ7:: [Int]->[Int]->([Int],[Int])
funcQ7 a b = (a,funcQ7c a b 0)

--{--Questão 8-----------------------------------------------}
type Filial = (String,Bool,[Int])

matriz :: [Filial]
matriz = [("Meta",True,[30,30,30,30,30,30,30,30]),("Hyperx",False,[34,2,3,4,5,3,2,35]),("Intel",True,[65,15,43,38,35,30,53,34]),("Amd",False,[10,24,54,32,1,3,0,0]),("Nvidea",True,[54,5,4,31,4,6,7,8]),("CS:GO",False,[09,6,55,32,45,76,42,35]),("Logitech",True,[01,0,0,1,23,3,5,6]),("GamersClub",False,[56,53,35,30,41,51,61,6])] 

funcQ8nome:: Filial -> String
funcQ8nome (a,b,c) = a

funcQ8Bool:: Filial -> Bool
funcQ8Bool (a,b,c) = b

funcQ8int::Filial -> [Int]
funcQ8int (a,b,c) = c


--Item 8.a
funcQ8Filiais::[Filial] ->[String]
funcQ8Filiais (a:b) = funcQ8FiliaisAux b

funcQ8FiliaisAux:: [Filial] -> [String]
funcQ8FiliaisAux [] = []
funcQ8FiliaisAux (a:b)
  |(funcQ8Bool a) = (funcQ8nome a) : (funcQ8FiliaisAux b)
  |otherwise = (funcQ8FiliaisAux b) 

funcQ8Candidata::[Filial] ->[String]
funcQ8Candidata (a:b) = funcQ8CandidataA b

funcQ8CandidataA::[Filial] -> [String]
funcQ8CandidataA [] = []
funcQ8CandidataA (a:b)
  |(funcQ8Bool a) /= True = (funcQ8nome a) : (funcQ8CandidataA b)
  |otherwise = (funcQ8CandidataA b) 

--Item 8.b 
funcComparaProducao:: [Int] -> [Int] -> Int
funcComparaProducao [] [] = 0
funcComparaProducao (x:y) (a:b)
 |a < x = (funcComparaProducao y b) + 1 
 |otherwise = funcComparaProducao y b

funcQ8bAux::Filial -> Filial -> Int
funcQ8bAux z x
 |(funcComparaProducao (funcQ8int z) (funcQ8int x)) >= 6 = 1
 |otherwise = 0

funcQ8bAux2::Filial -> [Filial] -> String
funcQ8bAux2 _ [] = "Todas as empresas bateram a meta"
funcQ8bAux2  z (x:y)
 |(funcQ8bAux z x) > 0 = "Houve empresa que nao bateu a meta"
 |otherwise = funcQ8bAux2 z y

funcQ8b::[Filial] -> String
funcQ8b (x:y) = funcQ8bAux2 x y

--Item 8.c
funcQ8cAux2:: Filial -> Filial -> String
funcQ8cAux2 z x
 |funcQ8bAux z x > 0 = "--- A empresa: "++(funcQ8nome x)++" nao bateu a meta ---"
 |otherwise =  "--- A empresa: "++(funcQ8nome x)++" bateu a meta ---"

funcQ8cAux:: Filial -> [Filial] -> String
funcQ8cAux _ [] = ""
funcQ8cAux z (x:y) = (funcQ8cAux2 z x)++(funcQ8cAux z y)

funcQ8c::[Filial] -> String
funcQ8c (x:y) = funcQ8cAux x y 

--Item 8.d

funcQ8dAux2:: Filial -> Filial -> String
funcQ8dAux2 z x
 |funcQ8bAux z x == 0 && funcQ8Bool x == False = "--- A empresa: "++(funcQ8nome x)++" sera promovida a Filial ---"
 |funcQ8bAux z x == 1 && funcQ8Bool x == False = "--- A empresa: "++(funcQ8nome x)++" permanecera candidata ---"
 |otherwise = "--- A empresa: "++(funcQ8nome x)++" ja eh uma filial ---"


funcQ8dAux:: Filial -> [Filial] -> String
funcQ8dAux _ [] = ""
funcQ8dAux z (x:y) = (funcQ8dAux2 z x)++(funcQ8dAux z y)


funcQ8d::[Filial] -> String
funcQ8d (x:y) = funcQ8dAux x y

--Item 8.e

-- O segundo campo dessa nova matriz usa um numero inteiro para reservar o estado da empresa (filial ou candidata), onde:
-- 0: é a posição da meta anual a ser alcançada
-- numeros impares: filiais
-- numeros pares: candidatas
-- 1 - 2: numeros reservados para a empresa 1, no momento ela é filial, caso seja rebaixada a candidata, basta somar 1 no inteiro, assumindo valor 2 (candidata)
-- 3 - 4: numeros reservados para a empresa 1, no momento ela é candidata, caso seja promovida a filial, basta subtrair 1 no inteiro, assumindo valor 3 (candidata)
-- e assim por diante, onde nunca o segundo campo sera repetido entre as empresas.

type FiliaisB = (String, Int, [Int])

funcQ8Matriz2 :: Int -> [FiliaisB]
funcQ8Matriz2 1 = [("Meta",0, [5,5,5,5,5,5,5,5,5,5]),("Empresa 1",1, [5,4,3,5,4,2,1,4,3,1]),("Empresa 2",4, [2,3,4,1,2,4,5,6,4,3]),("Empresa 3",5, [1,2,1,2,3,4,1,5,3,6]),("Empresa 4",7, [6,5,4,3,5,6,7,6,4,3]),("Empresa 5",10, [7,8,7,6,7,8,7,6,7,8]),("Empresa 6",12, [1,2,3,6,5,6,5,6,7,8]),("Empresa 7",13, [6,5,7,3,2,1,2,3,3,2])]


--{--Questão 9-----------------------------------------------}
funcQ9:: String -> Int
funcQ9 [] =0
funcQ9 (a:b)
  |isDigit a = (funcQ9 b) +1
  |otherwise = (funcQ9 b) +0

--{--Questão 10-----------------------------------------------}
--funcQ10 :: [Int]->Int -> Bool
--funcQ10 [] _ = False
--funcQ10 (a:b) x  
--  |x == a = (funcQ10 b x) || True
--  |otherwise = (funcQ10 b x) || False
funcQ10Aux::[Int] -> Int -> Int -> Bool
funcQ10Aux [] _ _ = False
funcQ10Aux (x:y) z c
 |x==z && x==c  = True
 |otherwise = (funcQ10Aux y z (c+1))


funcQ10::[Int] -> Int -> Bool
funcQ10 x y
 |y < 0 = False
 |y > (length x) = False
 |otherwise = funcQ10Aux x y 0

--{--Questão 11-----------------------------------------------}
funcQ11::[(Int,Int)]-> Int -> [Bool]
funcQ11 [] _ = []
funcQ11 ((a,b):c) n 
  |(a+b) > n = (True : (funcQ11 c n))
  |otherwise = (False: (funcQ11 c n))

--{--Questão 12-----------------------------------------------}
funcQ12:: Int  -> Int -> ([Int],[Int])
funcQ12 a b = (funcQ12div(funcQ12cria a b),funcQ12ndiv(funcQ12cria a b))

funcQ12cria::Int-> Int -> ([Int],[Int],Int)
funcQ12cria x y = ([x..y],[x..y],x)

funcQ12div::([Int],[Int],Int) -> [Int]
funcQ12div ([],_,_) = []
funcQ12div ((a:b),c,x) 
  |(a `mod`x) == 0 = a:(funcQ12div (b,c,x))
  |otherwise = (funcQ12div (b,c,x))

funcQ12ndiv::([Int],[Int],Int) -> [Int]
funcQ12ndiv (_,[],_) = []
funcQ12ndiv (a,(c:d),x) 
  |(c `mod`x) /= 0 = c:(funcQ12ndiv (a,d,x))
  |otherwise = (funcQ12ndiv (a,d,x))

--{--Questão 13-----------------------------------------------}
funcQ13::String -> String
funcQ13 a = (funcQ13Aux1 a (length a))

funcQ13Aux1:: String -> Int -> String
funcQ13Aux1 s 1 = s
funcQ13Aux1 s x = s++(funcQ13Aux1 s (x-1))

--{--Questão 14-----------------------------------------------}
--Item 14.a 
--Função de alta ordem é uma função que recebem uma ou mais funções como argumentos ou que devolvem outra função como valor de retorno.

--Item 14.b 
--A vantagem de se utilizar funções de alta ordem é que podemos reaproveitar outras funções que serão passadas por parametro para a função de alta ordem.

--Item 14.c 
--A Avaliação Preguiçosa é um recurso que permite a avaliação de expressões sob demanda. Ou seja, uma expressão somente será avaliada se (e somente se) for necessária, no decorrer da execução do programa.
--Esse recurso possibilita a seleção daquilo que será executado, independente da quantidade de expressões presentes em um programa.
--Por exemplo uma definição de lista infinita, somente os elementos necessários serão avaliados. Assim, uma lista infinita – obviamente – não terá que computar todos os seus elementos (o que levaria a quebrar o sistema), mas, somente, os que forem requisitados durante a execução do programa.

--{--Questão 15-----------------------------------------------}
type Pessoa  = [(Int, String, String)]
type Conhece = [(Int,[Int])]

agenda:: Pessoa
agenda = [(0,"vinicius","0000-0000"),(1,"marcio","1111-1111"),(2,"carlos","2222-2222"),(3,"joao","3333-3333"),(4,"lucas","4444-4444"),(5,"fanton","5555-5555"),(6,"eduardo","6666-6666")]

amigos :: Conhece
amigos =[(0,[1,2]),(1,[0,2,4]),(2,[0,1]),(3,[4,1]),(4,[3]),(5,[]),(6,[1,4,2])]


--Item 15.a 
funcQ15a:: String -> String
funcQ15a s  = s++" conhece "++(show (idConhece (nomeId s agenda) amigos))++ " pessoas" 

nomeId ::String ->Pessoa -> Int
nomeId _ [] = -1
nomeId  s ((a,b,c):x)
  |s == b = a
  |otherwise = nomeId s x

idConhece::Int -> Conhece -> Int
idConhece _ [] = 0
idConhece i ((a,b):d)
  |i == a = length(b)
  |otherwise = idConhece i d

--Item 15.b 
funcQ15b:: String -> [String]
funcQ15b a =(nomeConheceAux (nomeId a agenda) amigos)

nomeConheceAux:: Int -> Conhece -> [String]
nomeConheceAux _ [] = []
nomeConheceAux x  ((a,b):d) = (funcQ15bAux x a b)++(nomeConheceAux x d)

funcQ15bAux :: Int ->Int -> [Int] -> [String]
funcQ15bAux _ _ [] = []
funcQ15bAux x a (b:c) 
  |x == b = [(idNome a agenda)] 
  |otherwise = funcQ15bAux x a c

idNome::Int->Pessoa -> String
idNome _ [] = ""
idNome a ((x,y,z):n)
  |a == x = y
  |otherwise = idNome a n

--Item 15.c 
nomeTel ::String ->Pessoa -> String
nomeTel _ [] = ""
nomeTel  s ((a,b,c):x)
  |s == b = c
  |otherwise = nomeTel s x

--Item 15.d
funcQ15d:: String -> [String]
funcQ15d s = funcQ15djunta(listaLista (funcQ15conhece (nomeId s agenda) amigos) (todosId agenda))

todosId:: Pessoa ->[Int] -- todos os id
todosId [] = []
todosId ((x,y,z):n) = x : todosId n

funcQ15conhece::Int->Conhece -> [Int] -- meu id e de quem eu conheco
funcQ15conhece _ [] = []
funcQ15conhece x ((a,b):d) 
  |x==a = x:b 
  |otherwise = funcQ15conhece x d

funcQ15djunta::[Int]->[String]
funcQ15djunta [] = []
funcQ15djunta (a:b) = (idNome a agenda):(funcQ15djunta b)

listaLista::[Int]->[Int]->[Int]
listaLista [] [] = []
listaLista [] c = c
listaLista (a:b) c= listaLista b (intLista a c)

intLista:: Int -> [Int] -> [Int]
intLista _ [] = []
intLista x (a:b)
  |x/=a = a:(intLista x b)
  |otherwise= (intLista x b)

--Item 15.e
func15Conhecidos:: Conhece -> [Int]
func15Conhecidos [] = []
func15Conhecidos ((a,b):d) = b++(func15Conhecidos d)

func15eAux::[Int]
func15eAux = listaLista(func15Conhecidos amigos)(todosId agenda)

func15eAux2::[Int] -> [String]
func15eAux2 [] = []
func15eAux2 (a:b) = (idNome a agenda) :(func15eAux2 b)

func15e::[String]
func15e = func15eAux2 func15eAux


--{--Questão 16-----------------------------------------------}
funcQ16Aux:: Int -> String -> Char
funcQ16Aux 0 (x:y) = x  
funcQ16Aux z (x:y) = funcQ16Aux (z-1) y

funcQ16:: [(Int,String)] -> String
funcQ16 [] = ""
funcQ16 ((a,b):c) = (funcQ16Aux a b):(funcQ16 c)

--{--Questão 17-----------------------------------------------}
--Dada um valor de inteiro, retorna a lista dos naturais ate esse numero inteiro
funcQ17::Int->[Int]
funcQ17 a = [0..a]

--{--Questão 18-----------------------------------------------}
f::Int->Bool
f a
 |a == -41 = True
 |otherwise = False

filtraAux::Bool -> Int-> Int
filtraAux a b = b


filtraElimina::(Int->Bool) -> [Int] -> [Int]
filtraElimina f x = [filtraAux (f z) z| z <- x, (f z) == False]

--{--Questão 19-----------------------------------------------}
funcQ19Mod2::Int -> Int -> [Int] -> [Int]
funcQ19Mod2 1 x [] = [x | (x`mod`2==0)]
funcQ19Mod2 c z (x:y)
 |(z `mod` 2) == 0 = z:(funcQ19Mod2 (c-1) x y)
 |otherwise = (funcQ19Mod2 (c-1) x y)

funcQ19Mod3::Int -> Int -> [Int] -> [Int]
funcQ19Mod3 1 x [] = [x | (x`mod`3==0)]
funcQ19Mod3 c z (x:y)
 |(z `mod` 3) == 0 = z:(funcQ19Mod3 (c-1) x y)
 |otherwise = funcQ19Mod3 (c-1) x y

funcQ19Aux::Int -> Int -> [Int] -> [Int]
funcQ19Aux 1 x [] = [x | (x`mod`2/=0 && x`mod`3/=0)]
funcQ19Aux c z (x:y)
 |(z `mod` 2) /= 0 && (z `mod` 3) /= 0  = z:(funcQ19Aux (c-1) x y)
 |otherwise = funcQ19Aux (c-1) x y

funcQ19::[Int] -> ([Int],[Int],[Int])
funcQ19 (x:y) = (funcQ19Mod2 (length(x:y)) x y,funcQ19Mod3 (length(x:y)) x y,funcQ19Aux (length(x:y)) x y)

--{--Questão 20-----------------------------------------------}
funcQ20:: [[Int]] -> [Int]
funcQ20 [] = []
funcQ20 (a:b) = (funcQ20maior a ): (funcQ20 b)

funcQ20maior::[Int] -> Int
funcQ20maior [a] = a
funcQ20maior (a:b) 
  | a > (funcQ20maior b) = a
  |otherwise = (funcQ20maior b)

--{--Questão 21-----------------------------------------------}
funcQ21:: Char -> String -> [Bool]
funcQ21 _ [] = []
funcQ21 z (x:y)
 |z==x = True:(funcQ21 z y)
 |otherwise = False:(funcQ21 z y)

--{--Questão 22-----------------------------------------------}
funcQ22 :: [Int] -> Int -> [(Int,Int)]
funcQ22 x y = [(a,y) | a<-x, a>y]

--{--Questão 23-----------------------------------------------}
funcQ23Aux::(Eq a) => [a] -> [a]
funcQ23Aux [] = []
funcQ23Aux (x:y) 
  |(funcQ23Compara x y) /= [] = x:(funcQ23Aux y)
  |otherwise = (funcQ23Aux y)

funcQ23Compara::(Eq a) => a -> [a] -> [a]
funcQ23Compara _ [] = []
funcQ23Compara x (y:z) 
  |x == y = [x]
  |otherwise = funcQ23Compara x z
  
func23Tira::(Eq a) => [a] -> [a]
func23Tira [] = []
func23Tira (x:y) = x:(func23Tira (func23TiraAux x y) )

func23TiraAux::(Eq a) => a -> [a] -> [a]
func23TiraAux _ [] = []
func23TiraAux z (x:y)
 |z==x = func23TiraAux z y
 |otherwise = x:func23TiraAux z y

funcQ23::(Eq a) => [a] -> [a]
funcQ23 s = func23Tira (funcQ23Aux s) 

--{--Questão 24-----------------------------------------------}
terminaEm::Int -> [Int]
terminaEm a = [ x | x<-[a..100], x`mod`10 == a]

--{--Questão 25-----------------------------------------------}
type ListaDuplas = [(Int,Int)]
--Item 25.a
funcQ25a:: Int -> (Int,Int) -> Int
funcQ25a z (x,y)
 |abs (z-x) < abs (z-y) = x
 |otherwise = y

infix 7 &&&
(&&&)::Int->(Int,Int)->Int
x &&& y = funcQ25a x y

--Item 25.b
f1::Int->(Int,Int)->Int
f1 z (x,y) = x+z+y

funcQ25b::(Int->(Int,Int)->Int)->Int->ListaDuplas->[Int]
funcQ25b f1 x y = [f1 x z | z <- y] 

--Item 25.c
funcQ25c::Int -> ListaDuplas -> [Int]
funcQ25c x y = funcQ25b (&&&) x y


--{--Questão 26-----------------------------------------------}
mescla::String -> String -> [(Char,Int)]
mescla x y = mescla1 (funcTira x) y 

mescla1::String -> String -> [(Char,Int)]
mescla1 [] _ = []
mescla1 (a:b) s = (a,(funcQ26cont a s 0)): (mescla1 b s)

funcQ26cont::Char -> String-> Int -> Int
funcQ26cont c s n = length[ (n+1) | x<-s, c== x ]

funcTira::String -> String
funcTira [] = ""
funcTira (x:y) = x:(funcTira (funcTiraAux x y) )

funcTiraAux::Char -> String -> String
funcTiraAux _ [] = ""
funcTiraAux z (x:y)
 |z==x = funcTiraAux z y
 |otherwise = x:funcTiraAux z y

--{--Questão 27-----------------------------------------------}
maior::Int->Int->Int
maior a b
  |a > b = a
  |otherwise = b

menor::Int->Int->Int
menor a b
  | a < b = a
  |otherwise = b

infix 3 -*-
(-*-)::(Int,Int)->(Int,Int)->(Int,Int)
(x,y) -*- (a,b) = (maior (maior x y) (maior a b),menor (menor x y) (menor a b))


--{--Questão 28-----------------------------------------------}
--Item 28.a
funcQ28a:: Int -> [Int] -> Int
funcQ28a  _ [] = 0
funcQ28a a  (b:c)
  |a == b    = (funcQ28a a c) +1
  |otherwise = (funcQ28a a c) +0

ocorrencia:: Int -> [Int] -> (Int,Int)
ocorrencia a (b:c) = (a,(funcQ28a a (b:c)))

--Item 28.b 
aplica::(Int->[Int]->(Int,Int))->[Int]->[(Int,Int)]
aplica f k = [ (f a k) | a <- k]

--Item 28.c
funcQ28c::[Int]->[(Int,Int)]
funcQ28c x = aplica ocorrencia x

--{--Questão 29-----------------------------------------------}
funny x y z
  | x > z = True
  | y >= x = False
  | otherwise = True

funcQ29:: Int->Int->Int->Bool
funcQ29 x y z = x > z || y < x


--{--Questão 30-----------------------------------------------}
funcQ30::Int -> [Int] -> [Int]
funcQ30 _ [] = []
funcQ30 n (a:b)
  |n == a = b
  |otherwise = (a):(funcQ30 n (b))