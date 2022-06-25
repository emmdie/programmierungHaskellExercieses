isPrefix :: String -> String -> Bool
isPrefix "" x = True
isPrefix x "" = False
isPrefix prefix kette = (head prefix == head kette ) && isPrefix (drop 1 prefix) (drop 1 kette)

return1WhenTrue :: Bool -> Int
return1WhenTrue x
  | x == True = 1
  | x /= True = 0

countPattern :: String -> String -> Int
countPattern xs ys
 |  xs == "" && ys == "" = 1
 |  ys == "" = 0
 |  xs == "" = 1 + (countPattern xs (tail ys))
 | otherwise = (return1WhenTrue (xs `isPrefix` ys)) + (countPattern xs (tail ys))


