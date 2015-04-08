-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 6, Questão 1-}
    {-Eq and Show Graph-}
data Graph t = NilG
               | Node t [(t, Int)] (Graph t)

checkTerm :: (Eq t) => [(t, Int)] -> (t, Int) -> Bool
checkTerm list x | list == [] = False
                 | (head list) == x = True
                 | otherwise = checkTerm (tail list) x

contains :: (Eq t) => [(t, Int)] -> [(t, Int)] -> Bool
contains [] [] = True
contains [] y = False
contains x [] = False
contains (x:xs) y | xs == [] = (checkTerm y x)
               | otherwise = (checkTerm y x) && (contains xs y)

check :: (Eq t) => [(t, Int)] -> [(t, Int)] -> Bool
check x y = contains x y && contains y x

compareGraph :: (Eq t) => Graph t -> Graph t -> Bool
compareGraph (NilG) (NilG) = True
compareGraph (NilG) (Node _ _ _) = False
compareGraph (Node _ _ _) (NilG) = False
compareGraph (Node id1 list1 graph1) (Node id2 list2 graph2) = (id1 == id2) && (check list1 list2) && (compareGraph graph1 graph2)

showList :: (Show t) => [(t, Int)] -> String
showList [] = ""
showList ((n, w):xs) = (" - " ++ (show n) ++ " " ++ (show w)) ++ Main.showList xs

showGraph :: (Show t) => Graph t -> String
showGraph (NilG) = ""
showGraph (Node id list graph) = ((show id) ++ (Main.showList list)) ++ ". " ++ showGraph graph

{-Trabalho 6, Questão 2-}
	{-DFS in Graph-}
type UnweightedGraph = [(Int, [Int])]

dfs :: UnweightedGraph -> Int -> Bool
dfs [] k = False
dfs g k | fst(head(g)) == k = True
        | otherwise = loopdfs g (makeBool(g)) 1 k (length(snd(head(g))))

loopdfs :: UnweightedGraph -> [Bool] -> Int -> Int -> Int -> Bool
loopdfs [] v a k i = False
loopdfs g v a k 0 = False
loopdfs g v a k n | fst(g!!(a-1)) == k = True
                  | (v!!((snd(g!!(a-1))!!(n-1))-1)) == False = loopdfs g (turnBool v ((snd(g!!(a-1))!!(n-1))-1)) ((snd(g!!(a-1)))!!(n-1)) k (length(snd(g!!(((snd(g!!(a-1)))!!(n-1)-1))))) || loopdfs g v a k (n-1)
				  | otherwise = loopdfs g v a k (n-1)
				  
makeBool :: UnweightedGraph -> [Bool]
makeBool [] = []
makeBool g = False : makeBool (tail(g))

turnBool :: [Bool] -> Int -> [Bool]
turnBool [] n = []
turnBool b n = (take n b) ++ [True] ++ (drop (n+1) b)