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

-- funcao que faca uma lista de lista, na qual temos no elemento 0 dia e a venda
listaDiaVenda :: Int -> [[Int]]
----------------bases equivalentes-----------
--listaDiaVenda  (-1) = []
--listaDiaVenda 0 = (0:[vendas 0]):[]
listaDiaVenda 0 = (0:(vendas 0):[]) : []
---------------------------inducoes equivalentes-----------------------
--listaDiaVenda x =  [x,vendas x] : listaDiaVenda (x-1)
--listaDiaVenda x =  (x:[vendas x]) :  listaDiaVenda (x-1)
--listaDiaVenda x =  [[x,vendas x]] ++  (listaDiaVenda (x-1))
listaDiaVenda x =  ((x):(vendas x):[]) :  (listaDiaVenda (x-1))
-- ++ concatena listas (lista ++ lista)
-- : constroi lista (elemento : lista)


-- funcao que retorna uma lista de vendas
listVendas :: Int -> [Int]
listVendas  (-1) = []
listVendas x = (vendas x) : (listVendas (x-1))

-- funcao que retorna uma lista de vendas
listVendasR :: Int -> [Int]
listVendasR  8 = []
listVendasR x = (vendas x) : (listVendasR (x+1))

-- funcao que retorna uma lista de vendas
listVendasR2 :: Int -> [Int]
listVendasR2 (-1) = []
listVendasR2 x = (listVendasR2 (x-1)) ++ [vendas x]

-- funcao que retorna uma lista de vendas
listVendasR3 :: Int -> [Int]
listVendasR3  x
  |x > periodo = []
  |otherwise   = (vendas x) : (listVendasR (x+1))

-- funcao que receba a lista de vendas e retorne o total vendas
totalVendasList :: [Int]->Int
totalVendasList [] = 0
totalVendasList (a:b) =  a + (totalVendasList b)