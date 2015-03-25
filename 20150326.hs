-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

	{-Trabalho 3, Questão 1-}
	{-Defina HashTable como um tipo e faça operações sobre ele-}

{-
type HashTable = [(Int, Int)]

--pega um elemento dado uma chave
get :: HashTable -> Int -> Int
get [] key = 0
get ((k,v):as) key | k == key = v
                   | otherwise = get as key
		   
--põe um par chave/elemento na hash
put :: HashTable -> Int -> Int -> HashTable
put [] key val = [(key, val)]
put ((k,v):as) key val | k /= key = (k,v) : put as key val
                       | otherwise = (k,v) : as

--tira um elemento dado uma chave
remove :: HashTable -> Int -> HashTable
remove [] key = []
remove ((k,v):as) key | k == key = as
                      | otherwise = (k,v) : remove as key
				  
--verifica se uma chave está na hash
hasKey :: HashTable -> Int -> Bool
hasKey [] key = False
hasKey ((k,v):as) key = k == key || hasKey as key
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
hasKey ht kk = [k | (k, v) <- ht, k == kk] /= []

full :: HashTable -> Int -> Int
full [] kk = 0
full ht kk = length [v | (k, v) <- ht, (k /= kk && v /= -1)]

{-operation methods-}
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk | hasKey ht kk == False = -1
          | otherwise = snd(ht!!posGet ht kk 0)

put :: HashTable -> Int -> Int -> HashTable
put [] kk vv = [(kk,vv)]
put ht kk vv | full ht kk == 10 = ht
             | otherwise = take (posPut ht kk 0) ht ++ (kk, vv) : drop ((posPut ht kk 0)+1) ht

remove :: HashTable -> Int -> HashTable
remove [] kk = []
remove ht kk | hasKey ht kk == False = ht
             | otherwise = take (posGet ht kk 0) ht ++ (-1,-1) : drop ((posGet ht kk 0)+1) ht
		
{-additional methods to find the position-}			 
posGet :: HashTable -> Int -> Int -> Int
posGet [] kk mm = 0
posGet ht kk mm | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
                | otherwise = posGet ht kk (mm + 3)

posPut :: HashTable -> Int -> Int -> Int
posPut [] kk mm = 0
posPut ht kk mm | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
			    | hasKey ht kk == False && snd(ht!!(mod ((mod kk 10) + mm) 10)) == -1 = mod ((mod kk 10) + mm) 10
				| otherwise = posPut ht kk (mm + 3)
				  
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