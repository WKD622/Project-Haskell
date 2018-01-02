
day1 :: (Integral a, Eq a) => a -> a
day1 x = headSum (convert x)

convert :: Integral a => a -> [a]
convert 0 = []
convert x = convert (x `div` 10) ++ [x `mod` 10]

headSum :: (Num a, Eq a) => [a] -> a
headSum [] = 0
headSum [x] = x 
headSum (x:xs) = if x == last xs then x + innerSum 0 (x:xs)
                                 else innerSum 0 (x:xs)

innerSum :: (Num a, Eq a) => a -> [a] -> a
innerSum sum [] = sum
innerSum sum [x] = sum
innerSum sum (x:xs) = if x == head xs then innerSum (sum + x) xs 
                                  else innerSum (sum) xs 