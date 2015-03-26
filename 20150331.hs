-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 2-}
	{-Sequência Look and Say-}
	
lookAndSay :: Int -> Int
lookAndSay 0 = 0
lookAndSay n | n == 1 = 1
             | otherwise = lookAndSayN (lookAndSay (n-1)) 1 0

lookAndSayN :: Int -> Int -> Int -> Integer
lookAndSayN 0 curr count = (10 * count) + curr
lookAndSayN n curr count | mod n 10 == curr = lookAndSayN (div n 10) curr (count + 1)
                         | otherwise = 100 * lookAndSayN (div n 10) (mod n 10) 1 + (10 * count) + curr
