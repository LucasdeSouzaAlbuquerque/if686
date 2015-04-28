-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 9, Questão 1-}
	{-exercícios-}
	
-------MAP
	
-- mapSqrTwo
sqrtTwo :: Float -> Float
sqrtTwo x = x ** (0.5)

mapSqrtTwo :: (Float -> Float) -> [Float] -> [Float]
mapSqrtTwo f x | x == [] = []
               | otherwise = (f (head x)) : mapSqrtTwo f (tail x)

-- posicaoAlfabeto
baseAlphabet :: [(Char, Int)]
baseAlphabet = [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',10),('k',11),('l',12),('m',13),('n',14),('o',15),('p',16),('q',17),('r',18),('s',19),('t',20),('u',21),('v',22),('w',23),('x',24),('y',25),('z',26)]

getPos :: [(Char, Int)] -> Char -> Int
getPos abc x | x == (fst (head abc)) = snd (head abc)
             | otherwise = getPos (tail abc) x

posicaoAlfabeto :: ([(Char, Int)] -> Char -> Int) -> [Char] -> [Int]
posicaoAlfabeto f x | x == [] = []
                    | otherwise = (f baseAlphabet (head x)) : (posicaoAlfabeto f (tail x))

-- mapCL
mapCL :: (t -> t) -> [t] -> [t]
mapCL f x = [(f new) | new <- x]

-------DADOS/FOLD
	
-- insertNode
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

insertNode :: (Ord t) => Tree t -> t -> Tree t
insertNode (NilT) n = (Node n (NilT) (NilT))
insertNode (Node x a b) n = if n >= x then (Node x a (insertNode b n))
                            else (Node x (insertNode a n) b)

-- criarArvore
criarArvore :: (Ord t) => [t] -> (Tree t -> t -> Tree t) -> Tree t
criarArvore li _ = foldr (\x t -> insertNode t x) (NilT) (reverse li)

-------FILTER
	
-- filterList
filterListSum :: [[Int]] -> Int -> [[Int]]
filterListSum x e = filter (\x -> (foldr (+) 0 x) > e) x

-- inter
inter :: (Eq t) => [t] -> [t] -> [t]
inter l1 l2 = filter (\x -> elem x l2) l1

-- diff
diff :: (Eq t) => [t] -> [t] -> [t]
diff l1 l2 = filter (\x -> not (elem x l2)) l1

-------FUNÇÕES COMPOSTAS
	
-- mapFilter
mapFilter :: (t -> Bool) -> [[t]] -> [[t]]
mapFilter _ [] = []
mapFilter f (x:xs) = [el | el <- x, f el == True] : mapFilter f xs

-- mapFold
myFold :: (t -> u -> u) -> u -> [t] -> u
myFold _ b [] = b
myFold f b (x:xs) = f x (myFold f b xs)

mapFold :: (t -> u -> u) -> [u] -> [[t] -> u]
mapFold _ [] = []
mapFold f l = (\x -> myFold f (head l) x) : mapFold f (tail l)

{-Trabalho 9, Questão 2-}
	{-geraFuncaoMenorCaminho-}
type Node t = t
type Edge t = (Node t, Node t, Int)
data Graph t = NilG
               | Graph [Node t] [Edge t] deriving (Eq, Show)

ordEdge :: [Edge Int] -> [Edge Int]
ordEdge [] = []
ordEdge ((x1,x2,d):xs) = [((x1 - 1),(x2 - 1),d),((x2 - 1),(x1 - 1),d)] ++ (ordEdge xs)

newRow :: Int -> Int -> [Int]
newRow l c = if l == 0 then []
             else if l == c then 0 : (newRow (l - 1) c)
             else (99) : (newRow (l - 1) c)

newMatrix :: Int -> Int -> [[Int]]
newMatrix l c = if c == 0 then []
                else (newRow l c) : (newMatrix l (c - 1))

calcPos :: [[Int]] -> Edge Int -> [[Int]]
calcPos m (i, e, d) = (take i m) ++ [((take e (m!!i)) ++ [d] ++ (drop (e + 1) (m!!i)))] ++ (drop (i + 1) m)

origDist :: [[Int]] -> [Edge Int] -> [[Int]]
origDist m [] = m
origDist m e = origDist (calcPos m (head e)) (tail e)

createMatrix :: Graph Int -> [[Int]]
createMatrix (NilG) = []
createMatrix (Graph n e) = origDist (newMatrix (length n) (length n)) (ordEdge e)

minDist :: [[Int]] -> Int -> Int -> Int -> Int
minDist m i j 0 = (m!!(i - 1))!!(j - 1)
minDist m i j k = min (minDist m i j (k - 1)) ((minDist m i k (k - 1)) + (minDist m k j (k - 1)))

geraFuncaoMenorCaminho :: Graph Int -> Int -> Int -> String
geraFuncaoMenorCaminho (NilG) _ _ = ""
geraFuncaoMenorCaminho g i e = (show i) ++ " " ++ (show e) ++ " - " ++ (show (minDist m i e (length m)))
                               where m = createMatrix g
							   
							   -- Exercícios
{-
Determine, sem usar o GHCi, os tipos das seguintes expressões:
1) foldr (:)
- foldr :: (a -> b -> b) -> b -> [a] -> b
- (:) :: c -> [c] -> [c]
- a = c
- b = [c]
- foldr (:) :: b -> [a] -> b
foldr (:) :: [c] -> [c] -> [c]
2) map.(.)
- map :: (a -> b) -> [a] -> [b]
- (.) :: (d -> e) -> (c -> d) -> (c -> e)
- . :: (g -> h) -> (f -> g) -> (f -> h)
- f = (d -> e)
- g = (a -> b)
- g = ((c -> d) -> (c -> e))
- h = ([a] -> [b])
- a = (c -> d)
- b = (c -> e)
map.(.) :: (f -> h)
map.(.) :: ((d -> e) -> ([a] -> [b]))
map.(.) :: ((d -> e) -> ([c -> d] -> [c -> e]))
3) foldr (+).(.).map
- foldr :: (a -> b -> b) -> b -> [a] -> b
- (+) :: Num c => c -> c -> c
- . :: (e -> f) -> (d -> e) -> (d -> f)
- (.) :: (h -> i) -> (g -> h) -> (g -> i)
- . :: (k -> l) -> (j -> k) -> (j -> l)
- map :: (m -> n) -> [m] -> [n]
---------- (.).map ----------
- j = (m -> n)
- k = (h -> i)
- k = ([m] -> [n])
- l = (g -> h) -> (g -> i)
- h = [m]
- i = [n]
((.).map) :: (j -> l)
((.).map) :: (m -> n) -> ((g -> h) -> (g -> i))
((.).map) :: (m -> n) -> ((g -> [m]) -> (g -> [n]))
---------- foldr (+) ----------
- a = c
- b = c
foldr (+) :: (Num c) => c -> [c] -> c
---------- (foldr (+)).((.).map) ----------
- d = (m -> n)
- e = ((g -> [m]) -> (g -> [n]))
- e = (Num c) => c
- f = (Num c) => [c] -> c
- c = ((g -> [m]) -> (g -> [n]))
(foldr (+)).((.).map) :: (d -> f)
(foldr (+)).((.).map) :: (Num c) => (m -> n) -> [c] -> c
(foldr (+)).((.).map) :: (Num ((g -> [m]) -> (g -> [n]))) => (m -> n) -> [(g -> [m]) -> (g -> [n])] -> ((g -> [m]) -> (g -> [n]))
4) map.map.foldr
- map :: (a -> b) -> [a] -> [b]
- . :: (d -> e) -> (c -> d) -> (c -> e)
- map :: (f -> g) -> [f] -> [g]
- . :: (i -> j) -> (h -> i) -> (h -> j)
- foldr :: (k -> l -> l) -> l -> [k] -> l
---------- map.foldr ----------
- h = (k -> l -> l)
- i = (l -> [k] -> l)
- i = (f -> g)
- j = ([f] -> [g])
- f = l
- g = ([k] -> l)
map.foldr :: (h -> j)
map.foldr :: (k -> l -> l) -> ([f] -> [g])
map.foldr :: (k -> l -> l) -> ([l] -> [[k] -> l])
---------- map.(map.foldr) ----------
- c = (k -> l -> l)
- d = ([l] -> [[k] -> l])
- d = (a -> b)
- e = ([a] -> [b])
- a = [l]
- b = [[k] -> l]
map.(map.foldr) :: (c -> e)
map.(map.foldr) :: ((k -> l -> l) -> ([a] -> [b]))
map.(map.foldr) :: ((k -> l -> l) -> ([[l]] -> [[[k] -> l]]))
5) map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))
---------- (foldr (++) [] [[1], [2]]) ----------
[1] :: Num t -> [t]
[2] :: Num t -> [t]
(foldr (++) [] [[1], [2]]) :: (Num t) => [t]
---------- (foldr (++) (foldr (++) [] [[1], [2]])) ----------
- foldr :: (a -> b -> b) -> b -> [a] -> b
- b = [t]
- a = [t]
(foldr (++) (foldr (++) [] [[1], [2]])) :: [a] -> b
(foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => [[t]] -> [t]
---------- (.) (foldr (++) (foldr (++) [] [[1], [2]])) ----------
(.) :: (b0 -> c0) -> (a0 -> b0) -> (a0 -> c0)
- a0 = t
- b0 = [[t]]
- c0 = [t]
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (a0 -> b0) -> (a0 -> c0)
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (t -> [[t]]) -> (t -> [t])
---------- map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) ----------
- map :: (c -> d) -> [c] -> [d]
- . :: (f -> g) -> (e -> f) -> (e -> g)
- e = (t -> [[t]])
- f = (t -> [t])
- f = (c -> d)
- g = ([c] -> [d])
- c = t
- d = [t]
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num t) => (t -> [[t]]) -> ([c] -> [d])
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num t) => (t -> [[t]]) -> ([t] -> [[t]])
6) (foldr).(.)$(!!)
- foldr :: (a -> b -> b) -> b -> [a] -> b
- . :: (d -> e) -> (c -> d) -> (c -> e)
- (.) :: (g -> h) -> (f -> g) -> (f -> h)
- $ :: (i -> j) -> i -> j
- (!!) :: [k] -> Int -> k
---------- (foldr).(.) ----------
- c = (g -> h)
- d = (f -> g) -> (f -> h)
- d = (a -> b -> b)
- e = (b -> [a] -> b)
- (f -> g) = a
- (f -> h) = (b -> b)
- f = h = b
(foldr).(.) :: (c -> e)
(foldr).(.) :: (g -> h) -> (b -> [a] -> b)
(foldr).(.) :: (g -> f) -> (f -> [f -> g] -> f)
---------- (foldr).(.)$(!!) ----------
- (i -> j) = (g -> f) -> (f -> [f -> g] -> f)
- i = [k] -> Int -> k
- i = (g -> f)
- j = (f -> [f -> g] -> f)
- g = [k]
- f = Int -> k
(foldr).(.)$(!!) :: j
(foldr).(.)$(!!) :: (f -> [f -> g] -> f)
(foldr).(.)$(!!) :: (Int -> k) -> [(Int -> k) -> [k]] -> (Int -> k)
-}