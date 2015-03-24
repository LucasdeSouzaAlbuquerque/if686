-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

	-- Trabalho II - MERGESORT
	{- Complexidade:
		Merge pega duas listas e vai inserindo os menores elementos numa terceira, tendo complexidade n (tamanho somado das listas passadas)
		MergeSort tem recursões que dividem a lista em duas, então, supondo que começa com tamanho 16, por exemplo, vai 16->8->4->2->1 em suas chamadas recursivas,
		ou seja, seguindo uma progressão de LOG N.
		
		Como Mergesort atua sobre Merge, a complexidade completa do MERGESORT é N*LOG N -}
	
-- split
mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [a] = [a]
mergesort a = merge (mergesort (take (div (length a) 2) a)) (mergesort (drop (div (length a) 2) a))
	
-- join/sort two lists	
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if (x <= y)
            then x : merge xs (y:ys)
			else y : merge (x:xs) ys

---------------------------------------------------------------------------------------------
			
	-- Trabalho II - HEAPSORT
	{- Complexidade: 
		Swap: N (varrer a lista)
		Large / Largest: Constante (apenas acessar)
		Heapify: LOG N (pois ele apenas pode descer na heap, e para uma heap completa de N elementos seu número de níveis é LOG N
		BuildHeap: N*logN (n chamadas sobre Heapify)
		
		Logo, a complexidade de Heapsort tem nível máximo N*logN -}

-- element swap
swap :: Int -> Int -> [Int] -> [Int]
swap i j xs | i == j    = xs
            | i < j     = (take i xs) ++ ((xs!!j):[]) ++ (drop (i+1) (take j xs)) ++ ((xs!!i):[]) ++ (drop (j+1) xs)
            | otherwise = (take j xs) ++ ((xs!!i):[]) ++ (drop (j+1) (take i xs)) ++ ((xs!!j):[]) ++ (drop (i+1) xs)

-- max (list[i], list[2*i+1])
large :: Int -> Int -> [Int] -> Int
large i l xs = if ((2*i+1) < l) && ((xs!!(2*i+1)) > (xs!!i))
               then (2*i+1)
               else i

-- max (large, list[2*i+2])
largest :: Int -> Int -> [Int] -> Int
largest i l xs = if ((2*i+2) < l) && ((xs!!(2*i+2)) > (xs!!(large i l xs)))
                 then (2*i+2)
                 else large i l xs

-- swap father with sons
heapify :: Int -> Int -> [Int] -> [Int]
heapify i l xs = if ((largest i l xs) /= i)
                 then heapify (largest i l xs) l (swap (largest i l xs) i xs)
                 else xs

-- basic heap building
buildHeap :: Int -> [Int] -> [Int]
buildHeap i xs | i <= 0    = heapify 0 (length xs) xs
               | otherwise = buildHeap (i-1) (heapify i (length xs) xs)

-- extension of the main call
hpsort :: Int -> [Int] -> [Int]
hpsort i xs = if (i == 1)
              then heapify 0 i (swap 0 i xs)
              else hpsort (i-1) (heapify 0 i (swap 0 i xs))

-- 'main'
heapsort :: [Int] -> [Int]
heapsort xs = hpsort ((length xs)-1) (buildHeap (div (length xs) 2) xs)

---------------------------------------------------------------------------------------------

	--Exercícios Aula 24/03/2015

--Algoritmo de Ordenação Quicksort (crescente) para Inteiros. Usar (Ord a) => [a] -> [a] para vários tipos.
	{-Usando compreensões de listas-}
--Dar uma olhada melhor no funcionamento
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (pivot:list) = quickSort [y | y <- list, y < pivot] ++ [pivot] ++ quickSort [y | y <- list, y >= pivot]

--menor e maior inteiros
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c | (a < b && b < c) = (a,c)
                 | (a < c && c < b) = (a,b)
				 | (b < a && a < c) = (b,c)
				 | (b < a && c < a) = (b,a)
				 | (c < a && a < b) = (c,b)
				 | (c < a && b < a) = (c,a)
	
--tripla ordenada
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) | (a < b && b < c) = (a, b, c)
                       | (a < c && c < b) = (a, c, b)
				       | (b < a && a < c) = (b, a, c)
				       | (b < a && c < a) = (b, c, a)
				       | (c < a && a < b) = (c, a, b)
				       | (c < a && b < a) = (c, b, a)
					 
--Definição de tipos Ponto e Reta
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

--Função que retorna a coordenada X de um ponto
xCoord :: Ponto -> Float
xCoord p = fst p

--Função que retorna a coordenada Y de um ponto
yCoord :: Ponto -> Float
yCoord p = snd p

--Função que diz se uma reta é vertical à partir das coordenadas X dos pontos que a definem
vertLine :: Reta -> Bool
vertLine r = (fst (fst r) == fst (snd r))

{-Função que retorna o Y de um ponto para que este pertença à uma reta à partir de uma coordenada X.
	Se a reta for vertical, o X permanece igual entre os dois pontos que definem a reta, causando uma divisão por 0.
		No caso, para um 'x' dado, existirão infinitos pontos nessa reta-}
pontoY :: Float -> Reta -> Float
pontoY x r = (((snd (snd r) - snd (fst r)) * (x - fst(fst r))) / (fst (snd r) - fst(fst r))) + snd(fst r)

{-BIBLIOTECA-}

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo =  [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr. Norrell"),  ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] pp = []
livros ((p,l):as) pp | pp == p = l : livros as pp
			         | otherwise = livros as pp

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] ll = []
emprestimos ((p,l):as) ll | ll == l = p : emprestimos as ll
                          | otherwise = emprestimos as ll
			
emprestado :: BancoDados -> Livro -> Bool
emprestado [] ll = False
emprestado ((p,l):as) ll = ll == l || emprestado as ll

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] pp = 0
qtdEmprestimos ((p,l):as) pp | pp == p = 1 + qtdEmprestimos as pp
				             | otherwise = qtdEmprestimos as pp

--funções sobre o banco
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] pp ll = [(pp, ll)]
emprestar ((p,l):as) pp ll | ll /= l = (p,l) : emprestar as pp ll
                           | otherwise = (p,l) : as

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] pp ll = []
devolver ((p,l):as) pp ll | p == pp && l == ll = as
                          | otherwise = (p,l) : devolver as pp ll
						  
--compreensão de listas

membroC :: [Int] -> Int -> Bool
membroC ls ii = [i | i <- ls, ii == i] /= []

livrosC :: BancoDados -> Pessoa -> [Livro]
livrosC ls pp = [l | (p, l) <- ls, pp == p]

emprestimosC :: BancoDados -> Livro -> [Pessoa]
emprestimosC ls ll = [p | (p, l) <- ls, ll == l]

emprestadoC :: BancoDados -> Livro -> Bool
emprestadoC ls ll = [p | (p, l) <- ls, ll == l] /= []

qtdEmprestimosC :: BancoDados -> Pessoa -> Int
qtdEmprestimosC ls pp = length [l | (p, l) <- ls, pp == p]

devolverC :: BancoDados -> Pessoa -> Livro -> BancoDados
devolverC ls pp ll = [(p, l) | (p, l) <- ls, pp /= p || ll /= l]