f :: Int -> Int -> Int
f 0 y = y
f x y = x + (f(x+1) (x+y))

