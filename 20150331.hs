-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 1-}
	{- Questão Teórica / Overloading -}

{-Em Haskell, onde se é utilizada a classe de tipos, a sobrecarga de função pode ser utilizada em tipos que não necessariamente são subtipos.
Ex.:
method :: (Eq a) => a -> a -> Bool
a poderia ser do tipo Int ou Char, ou até mesmo String, e o corpo do method não precisaria ser alterado se fosse necessário a chamada do método method para int e logo depois para String. A única restrição de tipos é que ambos os tipos necessitam fazer parte da classe de tipos definida no tipo do método.

Em Java é utilizada classe de objetos, onde a sobrecarga de uma função é realizada por meio da criação de vários métodos, de mesmo nome mas de assinaturas diferentes, que estão ou não numa mesma classe. Ao chamar o método, Java procura pela assinatura do método qual destes melhor se adapta ao contexto (tipo e número de parâmetros) e utiliza este para compilar.
Ex.:
public int sum (int a, int b){
    return a + b;
}

public double sum (double a, double b) {
    return a + b
}

public static void main (String args []){
    int ia = 1;
	int ib = 2;
	System.out.println(a+b); // o sistema escolheria a primeira função
	double da = 1.0;
	double db = 2.0;
	System.out.println(a+b); // o sistema escolheria a segunda função
}-}

---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 2-}
	{-Sequência Look and Say-}
	
--loop para calcular cada valor incremental da sequência
lookAndSay :: Int -> Int
lookAndSay 0 = 0
lookAndSay n | n == 1 = 1
             | otherwise = lookAndSayN (lookAndSay (n-1)) 1 0

--para calcular valores individuais
lookAndSayN :: Int -> Int -> Int -> Int
lookAndSayN 0 curr count = (10 * count) + curr
lookAndSayN n curr count | mod n 10 == curr = lookAndSayN (div n 10) curr (count + 1)
                         | otherwise = 100 * lookAndSayN (div n 10) (mod n 10) 1 + (10 * count) + curr
						 
---------------------------------------------------------------------------------------------
						 	
{-Trabalho 4, Questão 3-}
	{-Grafo-}					 
						 
type No = (Int, [Int])
type Grafo = [No]
 
caminho :: Grafo -> Int -> Int -> [Int]
caminho [] a b = []
caminho g a b | a == b = [a]
              | otherwise = shorten (loopCaminho g (generateBool(g)) a b (length(snd(g!!(a-1)))))
 
shorten :: [Int] -> [Int]
shorten [] = []
shorten g | g!!0 == g!!1 = drop 1 g
          | otherwise = g
 
loopCaminho :: Grafo -> [Bool] -> Int -> Int -> Int -> [Int]
loopCaminho [] v a b n = []
loopCaminho g v a b 0 = []
loopCaminho g v a b n | b == ((snd(g!!(a-1)))!!(n-1)) = [a, b]
                      | v!!((snd(g!!(a-1)))!!(n-1)) == False && loopCaminho g (atualizeV v ((snd(g!!(a-1)))!!(n-1))) ((snd(g!!(a-1)))!!(n-1)) b (length(snd(g!!((snd(g!!(a-1)))!!(n-1))))) /= [] = [a] ++ loopCaminho g (atualizeV v ((snd(g!!(a-1)))!!(n-1))) ((snd(g!!(a-1)))!!(n-1)) b (length(snd(g!!((snd(g!!(a-1)))!!(n-1)))))
					  | v!!((snd(g!!(a-1)))!!(n-1)) == False && loopCaminho g v a b (n-1) /= [] = [a] ++ loopCaminho g v a b (n-1)
                      | otherwise = []
                   
generateBool :: Grafo -> [Bool]
generateBool [] = []
generateBool (a:as) = False : generateBool(as)
 
atualizeV :: [Bool] -> Int -> [Bool]
atualizeV [] n = []
atualizeV ba n = (take n ba) ++ [True] ++ (drop (n+1) ba)						 
						 
---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 4-}
	{-Filtro Mediana-}
	
--"base", dá o tamanho do kernel
filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana [[]] n = [[]]
filtroMediana mt 1 = mt
filtroMediana mt n = medianaRows (mt) (div n 2) (length mt)

--varre as linhas da matriz
medianaRows :: [[Int]] -> Int -> Int -> [[Int]]
medianaRows [[]] n m = [[]]
medianaRows mt 0 m = mt
medianaRows mt n 0 = []
medianaRows mt n m = medianaColumns mt n ((length mt)-m) (length mt) : medianaRows mt n (m-1)

--varre as colunas da matriz, por linha
medianaColumns :: [[Int]] -> Int -> Int -> Int -> [Int]
medianaColumns [[]] n i m = []
medianaColumns mt 0 i m = mt!!i
medianaColumns mt n i 0 = []
medianaColumns mt n i m | (i <= n-1 || i == ((length mt)-n)) = mt!!i
                        | otherwise = (medianaSpot mt n i ((length mt)-m)) : (medianaColumns mt n i (m-1))
					
--chama a função para cada posição válida da matriz				
medianaSpot :: [[Int]] -> Int -> Int -> Int -> Int
medianaSpot [[]] n i j = 0
medianaSpot mt 0 i j = (mt!!i)!!j
medianaSpot mt n i j | (j <= n-1 || j == ((length mt)-n)) = (mt!!i)!!j
                     | otherwise = mediana(quicksort(vectorColumns mt n n i j))

-- varre as colunas ao redor do ponto para montar o vetor
vectorColumns :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
vectorColumns [[]] n m i j = []
vectorColumns mt n m i j | m + n == 0 = vectorRows mt m m i (j-n)
                         | otherwise = (vectorRows mt m m i (j-n)) ++ (vectorColumns mt (n-1) m i j)

-- varre as linhas ao redor do ponto para montar o vetor
vectorRows :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
vectorRows [[]] n m i j = []
vectorRows mt n m i j | m + n == 0 = (mt!!(i-n))!!j : []
                      | otherwise = ((mt!!(i-n))!!j) : (vectorRows mt (n-1) m i j)

-- quicksort para ordenação
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = (quicksort [y | y <- rest, y < pivot]) ++ [pivot] ++ (quicksort [y | y <- rest, y >=  pivot])
					  
-- calcula a mediana de um vetor
mediana :: [Int] -> Int
mediana [] = 0
mediana ls | mod (length ls) 2 == 0 = div(ls!!(div (length ls) 2) + ls!!((div (length ls) 2) - 1)) 2
           | otherwise = ls!!(div (length ls) 2)
