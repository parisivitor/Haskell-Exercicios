func::[Int] -> Int
func [] = -1
func [_] = -1
func (_:b:_) = b

reverso :: [Int] -> [Int]
reverso [] = [] -- base de indução (elemento de parada da induçao)
reverso (a:b) = (reverso b) ++[a] -- indução recursiva 


filtra:: Int -> [Int] -> [Int]
filtra _[] = []
filtra x (a:b)
  | x == a     =  filtra x b   
  | otherwise  =  (a:(filtra x b))