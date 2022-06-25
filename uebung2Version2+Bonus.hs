fac x = product [1..x]

bincoeff :: Int -> Int -> Int
bincoeff n k
 | k == 0 = 1
 | n >= k = (fac n) `div` ((fac k) * (fac(n-k)))

prod :: [Int] -> Int
prod x = product x

rev :: [Int] -> [Int]
rev [] = []
rev x = [last x] ++ rev (init x)

excl :: Int -> [Int] -> [Int]
excl x liste = [a | a<-liste, a /=x]

isOrd :: [Int] -> Bool
isOrd liste
 | liste == [] = True
 | length liste == 1 = True
 | otherwise = ((head liste) <= head (tail(liste))) && isOrd (tail(liste))

merge :: [Int]-> [Int] -> [Int]
merge liste1 [] = liste1
merge [] liste2 = liste2
merge liste1 liste2
 | head liste1 <= head liste2 = [head liste1] ++ merge (tail liste1) liste2
 | otherwise = [head liste2] ++ merge (tail liste2) liste1

appendNextCollatzNumber :: [Int] -> [Int]
appendNextCollatzNumber liste
  | even (last liste) = liste ++ [(last liste)`div`2]
  | otherwise = liste ++ [(last liste)*3 +1]

findSmallestCollatzNumber :: [Int] -> Int
findSmallestCollatzNumber liste
 | last liste == 1 = length liste
 | otherwise = findSmallestCollatzNumber(appendNextCollatzNumber(liste)) 

collatzListeFillTillLength :: [Int] -> Int -> [Int]
collatzListeFillTillLength liste länge
 |length liste >= länge = liste
 |otherwise = collatzListeFillTillLength (appendNextCollatzNumber liste) länge  

collatzListeLengthN :: Int -> [Int]
collatzListeLengthN n = collatzListeFillTillLength  [n] n

findLast1 :: [Int] -> Int
findLast1 liste
 | length liste <= 0 = 0
 | last liste == 1 = length liste
 | otherwise = findLast1 (init liste)

findBiggestCollatzNumber :: Int -> Int
findBiggestCollatzNumber n = findLast1(collatzListeLengthN n)

collatzKN :: Int -> String
collatzKN n = "Der kleinste Wert k(n) ist " ++ show(findSmallestCollatzNumber [n]) ++ ". Der groesste Wert ist " ++ show(findBiggestCollatzNumber n)


