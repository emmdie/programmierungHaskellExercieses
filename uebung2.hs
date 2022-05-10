fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac x = x * fac (x-1)

bincoeff :: Int -> Int -> Int
bincoeff n k
 | k == 0 = 1
 | k > n = error "Basismenge kleiner als Zahl der gewählten Elemente"
 | otherwise = (fac n)`div`((fac k) * fac (n-k))

prod :: [Int] -> Int
prod a
  | length a == 0 = 1
  | length a == 1 = head a
  | otherwise = head a * prod (tail a)

rev :: [Int] -> [Int]
rev [] = []
rev a =[last a] ++ rev (init a)

excl :: Int -> [Int] ->  [Int]
excl x ggList = [y | y <- ggList, y/=x]

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd ggList
  | length ggList == 1 = True
  | length ggList > 1 = head ggList <= head(tail(ggList)) && isOrd (drop 1 ggList)

merge :: [Int]->[Int]->[Int]
merge voll [] = voll
merge [] voll = voll
merge liste1 liste2
  | head liste1 <= head liste2 = [head liste1] ++ merge (drop 1 liste1) liste2
  | head liste1 > head liste2 = [head liste2] ++ merge liste1 (drop 1 liste2)

fibs :: Int -> [Int]
fibs 0 = [0]
fibs 1 = [0,1]
fibs x = fibs (x-1) ++ [last(fibs (x-1))+ last(init(fibs (x-1)))]
