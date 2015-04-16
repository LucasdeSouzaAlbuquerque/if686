{-listPartitioner-}
listPartitioner :: [Ord t] => [t] -> ([t]->[[t]])
listPartitioner [] = tabajara []
listPartitioner a = tabajara (quicksort (a))

tabajara :: [Ord t] => [t] -> [t] -> [[t]]
tabajara [] b = []
tabajara (a:as) b | as /= [] = [[y | y <- quicksort(b), y <= a]] ++ tabajara as [y | y <- b, y > a]
                  | otherwise = [[y | y <- quicksort(b), y <= a]] ++ [[y | y <- b, y > a]]
				 
quicksort :: [Ord t] => [t] -> [t]
quicksort [] = []
quicksort (pivot:rest) = quicksort[y | y <- rest, y <= pivot] ++ [pivot] ++ quicksort[y | y <- rest, y > pivot]