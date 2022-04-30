fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac x = x * fac(x-1)

sumFac :: Int -> Int
sumFac n m = 
