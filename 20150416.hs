---- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

{-listPartitioner-}
listPartitioner :: [Int] -> ([Int]->[[Int]])
listPartitioner [] = tabajara []
listPartitioner a = tabajara (quicksort (a))

tabajara :: [Int] -> [Int] -> [[Int]]
tabajara [] b = []
tabajara (a:as) b | as /= [] = [[y | y <- quicksort(b), y <= a]] ++ tabajara as [y | y <- b, y > a]
                  | otherwise = [[y | y <- quicksort(b), y <= a]] ++ [[y | y <- b, y > a]]
				 
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = quicksort[y | y <- rest, y <= pivot] ++ [pivot] ++ quicksort[y | y <- rest, y > pivot]

---------------------------------------------------------------------------------------------
--dada uma função f do tipo t->u->v e defina uma função do tipo u->t->v q se comporta como f mas recebe seus parâmetros de ordem inversa
revertParams k = (\u t -> (k t u))
--f = (\t u -> (t>10) && u == True) para testes

--dada uma lista de pares, defina uma função que devolve uma lista contendo apenas os primeiros elementos de cada par
listFst = (\a -> map (fst) a)

--dada uma lista de lista de números e um número n devolver uma lista contendo todas cujo comprimento seja maior que n
lengthList a = (\b -> filter(((>b).length) ) a)

--dada uma lista de listas, criar uma lista que contém todos os elementos das sub-listas da lista de entrada, mas removendo duplicação:
dup :: (Eq t) => [t] -> [t]
dup [] = []
dup (x:xs) | not (elem x xs) = (x:(dup (xs)))
           | otherwise = (dup (xs))

jointList :: (Eq t) => ([[t]] -> [t])
jointList = (\a -> dup(foldr(++) [] a))

--maprfoldr


---AULA 6

--Somar uma constante x à todos os elementos de uma lista de números
sumX :: Int -> ([Int] -> [Int]) 
sumX k = map (+k)

--Dada uma lista de números obter o maior da lista
maiorList :: [Int] -> Int
maiorList = maximum

data BinaryTree t = NilT | Node t (BinaryTree t) (BinaryTree t) deriving (Eq)

--Dada uma árvore binária, devolver uma função que, dada uma árvore binária, verifica se as duas árvores são isomórficas
isomorfTree :: (Eq t) => BinaryTree t -> (BinaryTree t -> Bool)
isomorfTree (NilT) = (== (NilT))
isomorfTree a = (== a)