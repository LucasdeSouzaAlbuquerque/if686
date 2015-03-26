-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

	{-Trabalho 3, Questão 1-}
	{-Defina HashTable como um tipo e faça operações sobre ele-}
	{-Upgrade sobre Hash com Lista-}
	{-Versão com HASH FINITA, COM SUBSITUIÇÃO EM CONFLITO-}
	
{-
type HashTable = [(Int, Int)]

--pega um elemento dado uma chave
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk | fst(ht!!(mod kk 10)) == kk = snd(ht!!(mod kk 10))
          | otherwise = -1
		   
--põe um par chave/elemento na hash
put :: HashTable -> Int -> Int -> HashTable
put [] kk vv = [(kk, vv)]
put ht kk vv = take (mod kk 10) ht ++ (kk, vv) : drop ((mod kk 10)+1) ht

--tira um elemento dado uma chave
remove :: HashTable -> Int -> HashTable
remove [] kk = []
remove ht kk | fst(ht!!(mod kk 10)) == kk = take (mod kk 10) ht ++ (-1, -1) : drop ((mod kk 10)+1) ht
             | otherwise = ht
			 
--verifica se uma chave está na hash
hasKey :: HashTable -> Int -> Bool
hasKey [] kk = False
hasKey ht kk = fst(ht!!(mod kk 10)) == kk
-}

---------------------------------------------------------------------------------------------

	{-Trabalho 3, Questão 1-}
	{-Defina HashTable como um tipo e faça operações sobre ele-}
	{-Versão com HASH FINITA, FUNÇÃO DE MÓDULO usando LINEAR PROBING, ATUALIZAÇÃO E DESLOCAMENTO-}
	
type HashTable = [(Int, Int)]

baseExemplo :: HashTable
baseExemplo = [(1,4),(11,4),(3,6),(4,2),(9,5),(8,3),(7,-1),(12,8),(16,3),(10,2)]
		 
{-base methods that check if the hash has the key or if it is full-}		 
hasKey :: HashTable -> Int -> Bool
hasKey [] kk = False
hasKey ht kk = posGet ht kk 0 0 /= -1

{-operation methods-}
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk | posGet ht kk 0 0 == -1 = -1
          | otherwise = snd(ht!!posGet ht kk 0 0)

put :: HashTable -> Int -> Int -> HashTable
put [] kk vv = [(kk,vv)]
put ht kk vv | posPut ht kk 0 0 == -1 = ht
             | otherwise = take (posPut ht kk 0 0) ht ++ (kk, vv) : drop ((posPut ht kk 0 0)+1) ht

remove :: HashTable -> Int -> HashTable
remove [] kk = []
remove ht kk | posGet ht kk 0 0 == -1 = ht
             | otherwise = take (posGet ht kk 0 0) ht ++ (-1,-1) : drop ((posGet ht kk 0 0)+1) ht
		
{-additional method to find the position-}			 
posPut :: HashTable -> Int -> Int -> Int -> Int
posPut [] kk mm x = 0
posPut ht kk mm x | x >= 20 = -1
                  | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
			      | x >= 10 && snd(ht!!(mod ((mod kk 10) + mm) 10)) == -1 = mod ((mod kk 10) + mm) 10
                  | otherwise = posPut ht kk (mm + 3) (x + 1)
			   
posGet :: HashTable -> Int -> Int -> Int -> Int
posGet [] kk mm x = 0
posGet ht kk mm x | x >= 10 = -1
                  | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
                  | otherwise = posGet ht kk (mm + 3) (x + 1)
				  
---------------------------------------------------------------------------------------------

	{-Trabalho 3, Questão 2-}
	{-Defina uma função "compara conjuntos" que diz se A contém B, B contém A, os dois tem alguma interseção, são iguais ou disjuntos-}

comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos a b | contem a b && contem b a = "A igual a B"
                     | contem b a = "A contem B"
                     | contem a b = "B contem A"
                     | intersecao a b = "A interseciona B"
					 | otherwise = "Conjuntos disjuntos"
					 
--verifica se todos os membros de A estão em B
contem :: (Eq t) => [t] -> [t] -> Bool
contem a b | a == [] = True
           | otherwise = (existe b (head a) && contem (tail a) b)

--verifica se algum membro de A está em B
intersecao :: (Eq t) => [t] -> [t] -> Bool
intersecao a b | a == [] = False
               | otherwise = (existe b (head a) || intersecao (tail a) b)

--verifica se existe um valor X em um conjunto
existe :: (Eq t) => [t] -> t -> Bool
existe xs s | xs == [] = False
            | otherwise = ((head xs) == s || existe (tail xs) s)
			
---------------------------------------------------------------------------------------------

{-Exercicios Sala de Aula-}

take :: [t] -> Int -> [t]
take [] n = []
take (a:as) 0 = []
take (a:as) n = a : Main.take as (n-1)

drop :: [t] -> Int -> [t]
drop [] n = []
drop (a:as) 0 = a : Main.drop as 0
drop (a:as) n = Main.drop as (n-1)

takeWhile :: [t] -> (t -> Bool) -> [t]
takeWhile [] k = []
takeWhile (a:as) k | k a == False = []
                   | otherwise = a : Main.takeWhile as k

dropWhile :: [t] -> (t -> Bool) -> [t]
dropWhile [] k = []
dropWhile (a:as) k | k a == False = a : Main.drop as 0
                   | otherwise = Main.dropWhile as k

-- order
order :: (Ord a) => [a] -> [a]
order [] = []
order (pivot:rest) = (order [y | y <- rest, y < pivot]) ++ [pivot] ++ (oder [y | y <- rest, y >= pivot])

-- group

