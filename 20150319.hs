-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

	{-Exercícios Aula 19/03/2015-}
	
--Import para uma versão da função 'digits', que extrai apenas os números de uma string
import Data.Char ( isNumber )

--Função que dobra todos os membros de uma lista
double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2 * x) : double xs

--Função que verifica se uma lista contém um membro
member :: [Int] -> Int -> Bool
member ls s | ls == [] = False
            | (head ls) == s = True
			| otherwise = member (tail ls) s

--Função que extrai os números de uma string
digits :: String -> String
digits = filter isNumber

--Outra versão de 'digits' sem utilizar o filtro
digits2 :: String -> String
digits2 a | a == [] = []
          | (head a >= '0' && head a <= '9') = [head a] ++ digits2 (tail a) 
		  | otherwise = digits2 (tail a)

--Função que soma os elementos de duas listas
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs [] (x:xs) = x : sumPairs [] xs
sumPairs (y:ys) [] = y : sumPairs ys []
sumPairs (y:ys) (x:xs) = x + y : sumPairs ys xs

--Algoritmo de Ordenação Quicksort (crescente) para Inteiros. Usar (Ord a) => [a] -> [a] para vários tipos.
--Dar uma olhada melhor no funcionamento
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (pivot:list) = quickSort [y | y <- list, y < pivot] ++ [pivot] ++ quickSort [y | y <- list, y >= pivot]
			
--Função que recebe um inteiro e retorna uma lista com 0 e os n primeiros membros de Fibonacci
fib :: Int -> [Int]
fib 0 = [0]
fib 1 = [1, 0]
fib n = head (fib (n-1)) + head (fib (n-2)) : fib (n-1)

--Função de Ordenação Quicksort baseada na soma dos dígitos. (crescente)
quickSortDigits :: [Int] -> [Int]
quickSortDigits [] = []
quickSortDigits (pivot:list) = quickSortDigits [y | y <- list, digitsSum (y) < digitsSum (pivot)] ++ [pivot] ++ quickSortDigits [y | y <- list, digitsSum (y) >= digitsSum (pivot)]	

--Função que soma os digitos de um número
digitsSum :: Int -> Int
digitsSum a | a == 0 = 0
            | otherwise = mod a 10 + digitsSum (div a 10)