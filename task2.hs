import Data.List

day2 :: Integral a => [a] -> a
day2 x = (sumDifference(listMin x))

sumDifference :: Num a => [a] -> a
sumDifference [] = 0
sumDifference (x:xs) = x + sumDifference (xs)

listMin :: Integral a => [a] -> [a] 
listMin [] = []
listMin (x:xs) = listMin (xs) ++ [diffrence (convert x)] 

diffrence :: (Num a, Ord a) => [a] -> a
diffrence [] = 0
diffrence xs = maximum xs - minimum xs

convert :: Integral a => a -> [a]
convert 0 = []
convert x = convert (x `div` 10) ++ [x `mod` 10]



