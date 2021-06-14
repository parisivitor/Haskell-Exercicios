periodo:: Int
periodo = 7

--tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30
vendas x = -1

--tabela de vendas2 equivalente a vendas
vendas2 :: Int -> Int
vendas2 x
  |x==0 = 0 --condicional de parada
  |x==1 = 41
  |x==2 = 72
  |x==3 = 48
  |x==4 = 0
  |x==5 = 91
  |x==6 = 55
  |x==7 = 30
  |otherwise  = -1


--funcao que retorna o total de vendas decrecente
totalvendas :: Int -> Int
totalvendas 0 = vendas 0  -- condicional de parada
totalvendas x = vendas x + (totalvendas (x-1)) --loop recursivo

--funcao que retorna o total de vendas crescente
totalvendas2 :: Int -> Int
totalvendas2 x
  |x==7 = vendas2 7  -- condicional de parada
  |otherwise = vendas2 x + (totalvendas2 (x+1)) --loop recursivo


totalVendasPeriodo = totalvendas periodo

-- funcao que retorna quantas vendas superam um valor 
diasSuperaValor::Int->Int->Int
diasSuperaValor x 0 = 0
diasSuperaValor x d
  | x < vendas d = 1 + diasSuperaValor x (d-1)
  | otherwise    = 0 + diasSuperaValor x (d-1)

dSValor :: Int -> Int
dSValor x  = diasSuperaValor x periodo

-- funcao maior
maior:: Int -> Int -> Int
maior a b
  |(a>b) = a
  |otherwise =b

--retorno maior venda
maiorVenda:: Int->Int
maiorVenda x
  |x==0 = vendas 0
  |otherwise = maior (vendas x) (maiorVenda(x-1))

maiorV :: Int
maiorV = maiorVenda periodo

-- retorna o dia de certa venda
diaVenda :: Int -> Int -> Int
diaVenda _(-1) = (-1)
diaVenda v d 
  |(vendas d) == v = d
  |otherwise = diaVenda v (d-1)

dV :: Int -> Int
dV x = diaVenda x periodo

diaMaiorVenda = dV maiorV