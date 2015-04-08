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
		   ---------------------------------------------------------------------------------------------------
{-Exercícios Aula Monitoria-}

{-
1 - Crie uma função que simule o comportamento de um AFD. 
Ela deve receber uma cadeia de caracteres (apenas 0s e 1s) e as informações do autômato: 
	sua lista de estados, lista de transições, estado inicial e lista de estados de aceitação.
A função deve retornar um Bool indicando se a cadeia foi aceita ou não.
-}

--tipo que representa a transição
type Trans = (Int, Int, String)

--main e casamento de padrões simples
afd :: String -> [Int] -> [Trans] -> Int -> [Int] -> Bool
afd "" q t s f = exist s f
afd a [] t s f = False
afd a q [] s f = False
afd a q t s [] = False
afd a q t s f = exist s f || afd (drop 1 a) q t (nextState a s t) f

--verifica se o estado atual está na lista de estados finais
exist :: Int -> [Int] -> Bool
exist n [] = False
exist n (a:as) = n == a || exist n as

--à partir da lista de transições, vai para o próximo estado
nextState :: [Char] -> Int -> [Trans] -> Int
nextState "" s (a:as) = s
nextState c s [] = s
nextState c s (a:as) | s == get1st(a) && [(c!!0)] == get3rd(a) = get2nd(a)
                     | otherwise = nextState c s as
					
--pega o primeiro elemento de uma tupla de 3
get1st :: Trans -> Int
get1st (a,b,c) = a

--pega o segundo elemento de uma tupla de 3
get2nd :: Trans -> Int
get2nd (a,b,c) = b
 
--pega o terceiro elemento de uma tupla de 3
get3rd :: Trans -> String
get3rd (a,b,c) = c

---------------------------------------------------------------------------------------------------

{-
2 - Implemente a função somatorioHexadecimal. 
Essa função recebe uma lista de Strings onde cada elemento representa um numero na base hexadecimal 
	e retorna uma String contendo o resultado em hexadecimal do somatório da primeira lista.
-}

--main do somatorio
somatorioHexadecimal :: [String] -> String
somatorioHexadecimal [] = ""
somatorioHexadecimal (a:as) = conversaoIda(somaLista (a:as))

--soma os elementos individuais da lista
somaLista :: [String] -> Int
somaLista [] = 0
somaLista (a:as) = conversaoVolta(a) + somaLista(as)

--converte de hexadecimal para int
conversaoVolta :: String -> Int
conversaoVolta "" = 0
conversaoVolta s | s!!((length s)-1) == 'A' = 10 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == 'B' = 11 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == 'C' = 12 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == 'D' = 13 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == 'E' = 14 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == 'F' = 15 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '0' = 0 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '1' = 1 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '2' = 2 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '3' = 3 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '4' = 4 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '5' = 5 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '6' = 6 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '7' = 7 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '8' = 8 + (conversaoVolta(take ((length s)-1) s))*16
                 | s!!((length s)-1) == '9' = 9 + (conversaoVolta(take ((length s)-1) s))*16

--converte de int para hexadecimal
conversaoIda :: Int -> String
conversaoIda 0 = ""
conversaoIda k | mod k 16 == 10 = conversaoIda(div k 16) ++ ['A']
               | mod k 16 == 11 = conversaoIda(div k 16) ++ ['B']
               | mod k 16 == 12 = conversaoIda(div k 16) ++ ['C']
               | mod k 16 == 13 = conversaoIda(div k 16) ++ ['D']
               | mod k 16 == 14 = conversaoIda(div k 16) ++ ['E']
               | mod k 16 == 15 = conversaoIda(div k 16) ++ ['F']
			   | otherwise = conversaoIda(div k 16) ++ show(mod k 16)  
			   
---------------------------------------------------------------------------------------------------

{-
3 - Escreva uma função palindromoDecimal :: String -> String que, 
	recebendo um número inteiro positivo N em hexadecimal (respresentado como um String), 
		seja capaz de verificar se este número, transformado para a base decimal, 
			é um palíndromo (não é permitido o uso da função reverse).
-}

--main
palindromoDecimal :: String -> String
palindromoDecimal "" = "0 - NAO-PALINDROMO"
palindromoDecimal s | mod(length s) 2 == 0 && palinPar(conversaoVolta(s))[] = show(conversaoVolta(s)) ++ " - PALINDROMO"
                    | mod(length s) 2 /= 0 && palinImpar(conversaoVolta(s))[] = show(conversaoVolta(s)) ++ " - PALINDROMO"
					| otherwise = show(conversaoVolta(s)) ++ " - NAO-PALINDROMO"

--calcula se um número de tamanho par é palindromo					
palinPar :: Int -> [Int] -> Bool
palinPar 0 [] = True
palinPar 0 ls = False
palinPar n ls | n >= 10 && n <= 100 && length(ls) == 0 = mod n 10 == div n 10
              | potTen(length(ls)) >= n && mod n 10 == ls!!((length ls)-1) = palinPar (div n 10) (take ((length ls)-1) ls)
              | otherwise = palinPar (div n 10) (ls++[(mod n 10)])
			  
--calcula se um número de tamanho impar é palindromo		
palinImpar :: Int -> [Int] -> Bool
palinImpar 0 [] = True
palinImpar 0 ls = False
palinImpar n ls | n < 10 && length(ls) == 0 = True
                | potTen(length(ls)) >= n && mod n 10 == ls!!((length ls)-1) = palinImpar (div n 10) (take ((length ls)-1) ls)
				| potTen(length(ls)) <= n && 10*potTen(length(ls)) >= n = palinImpar (div n 10) ls
                | otherwise = palinImpar (div n 10) (ls++[(mod n 10)])
		
--gera potências de 10
potTen :: Int -> Int
potTen 0 = 0
potTen 1 = 10
potTen k = 10 * potTen(k-1)		
	
---------------------------------------------------------------------------------------------------
--Defina o tipo de matrizes e multiplique-as--

-- multiplicaMatrizes
type Vector = [Double]
type Matrix = [Vector]

multElement :: Matrix -> Matrix -> Int -> Int -> Int -> Double
multElement m n a b c | m == [] || n == [] || b == -1 = 0
                      | otherwise = (((m!!a)!!b) * ((n!!b)!!c)) + (multElement m n a (b - 1) c)

multRow :: Matrix -> Matrix -> Int -> Int -> Vector
multRow m n x y | m == [] || n == [] || y == (length m) = []
                | otherwise = [multElement m n x (length m - 1) y] ++ (multRow m n x (y + 1))

auxMatrix :: Matrix -> Matrix -> Int -> Matrix
auxMatrix m n x | m == [] || n == [] || x == (length m) = []
          | otherwise = [multRow m n x 0] ++ (auxMatrix m n (x + 1))

multiplicaMatrizes :: Matrix -> Matrix -> Matrix
multiplicaMatrizes a b | (length a) == 0 || (length b) == 0 = []
                       | otherwise = auxMatrix a b 0
