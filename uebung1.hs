fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac x = x * fac(x-1)

sumFac :: Int -> Int -> Int
sumFac n m
 | m<n = 0
 | otherwise = sum[fac i|i<-[n..m]]

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo x = (fibo (x-1)) + (fibo (x-2))
