{-listPartitioner-}
listPartitioner :: [Int] -> ([Int]->[[Int]])
listPartitioner [] = tabajara []
listPartitioner a = tabajara (quicksort (a))

tabajara :: [Int] -> [Int] -> [[Int]]
tabajara [] b = []
tabajara (a:as) b | as /= [] = [[y | y <- quicksort(b), y <= a]] ++ tabajara as [y | y <- b, y > a]
                  | otherwise = [[y | y <- quicksort(b), y <= a]] ++ [[y | y <- b, y > a]]
				 
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = quicksort[y | y <- rest, y <= pivot] ++ [pivot] ++ quicksort[y | y <- rest, y > pivot]