{-Exercicios da Aula 02, Slide 19-}

--Funcao para calcular o número de dias cujo número de vendas é igual à um valor s
fun :: Int -> Int -> Int
fun s n | (n == 0 && vendas(0) == s) = 1
	    | (n == 0) = 0
        | (vendas n == s) = 1 + fun (s) (n-1)
        | otherwise = fun (s) (n-1)

--Funcao venda de exemplo		
vendas:: Int -> Int
vendas n = n * 2

{-Exercicios da Aula 03, Slide 10-}

fat :: Int -> Int
fat k | (k == 0) = 1
      | otherwise = k * fat (k-1)

--All4Equal normal
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b && b == c && c == d)

--AllEqual para usar no segundo All4Equal
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

--All4Equal usando AllEqual
all4Equal2 :: Int -> Int -> Int -> Int -> Bool
all4Equal2 a b c d = (allEqual (a) (b) (c) && allEqual (b) (c) (d))

--Conta o número de parâmetros iguais
equalCount :: Int -> Int -> Int -> Int
equalCount a b c | (allEqual (a) (b) (c)) = 3
                 | (a == b || b == c || a == c) = 2
                 | otherwise = 0
