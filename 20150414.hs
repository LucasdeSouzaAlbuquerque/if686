-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 7, Questão 1-}
    {-compose-}
compose :: (u -> v) -> [(t -> u)] -> [(t -> v)]
compose f fl = [f . (head fl)] ++ (compose f (tail fl))

	{-map and fold graph-}
data Graph t = NilG
               | Node t [(t, Int)] (Graph t)

mapList :: [(t, Int)] -> (t -> u) -> [(u, Int)]
mapList [] _ = []
mapList (x:xs) f = (f (fst x), (snd x)) : mapList xs f

mapGraph :: Graph t -> (t -> u) -> Graph u
mapGraph (NilG) _ = (NilG)
mapGraph (Node x l g) f = Node (f x) (mapList l f) (mapGraph g f)

auxF :: (Num t) => Graph t -> [t]
auxF (NilG) = []
auxF (Node x l g) = [x] ++ (auxF g)

foldGraph :: (Num t) => Graph t -> (t -> t -> t) -> t -> t
foldGraph (NilG) _ i = i
foldGraph x f i = foldr f i (auxF x)

-- graph's checking function
showList :: (Show t) => [(t, Int)] -> String
showList [] = ""
showList ((n, w):xs) = (" - " ++ (show n) ++ " " ++ (show w)) ++ Main.showList xs

showGraph :: (Show t) => Graph t -> String
showGraph (NilG) = ""
showGraph (Node id list graph) = ((show id) ++ (Main.showList list)) ++ ". " ++ showGraph graph

	{-binaryTreeFilter-}	
	
	-- Dada uma árvore, gera uma floresta em
-- que todos os nós obedecem o predicado.
filterNodes :: (t -> Bool) -> Tree t -> Tree t
filterNodes f NilT = NilT
filterNodes f (TNode n l r)
 | f n = (TNode n (filterNodes f l) (filterNodes f r))
 | otherwise = NilT

-- Encontra uma subárvore cuja raiz não obedece ao
-- predicado, e aplica filterTree nas subárvores da mesma.
findNextForest :: (t -> Bool) -> Tree t -> [Tree t]
findNextForest f NilT = []
findNextForest f (TNode n l r)
 | f n = (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

-- Dada uma árvore, verifica se a raiz obedece ao predicado.
-- Caso obedeça, gera a floresta desta, e concatena o resultado
-- com as florestas que possam existir nas subárvores à esquerda
-- e à direita (subárvores de uma árvore cuja raiz não obedece ao predicado).
-- Caso contrário, chama a função para as subárvores à esquerda e à direita. 
filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree f NilT = []
filterTree f t@(TNode n l r)
 | f n = [filterNodes f t] ++ (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

testTree :: Tree Int
testTree = (TNode 5 (TNode 7 (TNode 15 NilT (TNode 6 NilT NilT)) (TNode 2 NilT NilT)) (TNode 10 NilT NilT))
