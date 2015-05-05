-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

--e começa a brincadeira

---------------------------------------------------------------------------------------------

{-Trabalho 11, Questão 1-}

{-Trabalho 11, Questão 2-}
import Control.Monad

-- métodos auxiliares
-- getSpace
getSpace :: String -> Int
getSpace x | x == [] = 0
           | (head x) == ' ' = 0
           | otherwise = 1 + getSpace (tail x)

-- getWord
getWord :: String -> String
getWord x | x == [] = []
          | otherwise = take (getSpace x) x

-- dropWord
dropWord :: String -> String
dropWord x | x == [] = []
           | otherwise = drop (getSpace x) x

-- dropSpace
dropSpace :: String -> String
dropSpace x | x == [] = []
            | x!!0 /= ' ' = x
            | otherwise = dropSpace (tail x)

-- splitWords
splitWords :: String -> [String]
splitWords x | x == [] = []
             | ((getSpace x) == 0 && (x!!0 /= ' ')) = (getWord x):[]
             | otherwise = (getWord x) : (splitWords (dropSpace (dropWord x)))

strCheck :: String -> [Maybe Char]
strCheck [] = [Nothing]
strCheck (s:ss) = if ((fromEnum s) > 64 && (fromEnum s) < 91) || ((fromEnum s) > 96 && (fromEnum s) < 123) then (Just s) : (strCheck ss)
                  else (Nothing) : (strCheck ss)

strCaps :: [Maybe Char] -> [Maybe Char]
strCaps [] = []
strCaps ((Nothing):ss) = (Nothing) : (strCaps ss)
strCaps ((Just s):ss) = if (fromEnum s) > 96 then (Just (toEnum ((fromEnum s) - 32))) : (strCaps ss)
                        else (Just s) : (strCaps ss)

strPart :: [Maybe Char] -> String
strPart [] = []
strPart ((Nothing):ss) = ' ' : (strPart ss)
strPart ((Just s):ss) = s : (strPart ss)

main :: IO ()
main = do {
 input <- getLine;
 mapM_ putStrLn (splitWords $ strPart $ strCaps $ strCheck input);
 main
}