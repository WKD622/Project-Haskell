-- The first row's largest and smallest values are 9 and 1, and their difference is 8.
-- The second row's largest and smallest values are 7 and 3, and their difference is 4.
-- The third row's difference is 6.

import Data.List
import Test.HUnit

day2 :: Integral a => [a] -> a
day2 x = (sumDifference(listMin x))

-- Calculates sum of list elements (sum of differences): sumDifference [6,4,8] -> 18
sumDifference :: Num a => [a] -> a
sumDifference [] = 0
sumDifference (x:xs) = x + sumDifference (xs)

-- Calculates maximum digit differenc of each element in array:listMin  [5195,753,2468] -> [6,4,8]
listMin :: Integral a => [a] -> [a] 
listMin [] = []
listMin (x:xs) = listMin (xs) ++ [diffrence (convert x)] 

-- calculates diffrence between minimum and maximum of the array: diffrence [10,1] -> 9
diffrence :: (Num a, Ord a) => [a] -> a
diffrence [] = 0
diffrence xs = maximum xs - minimum xs

-- converts Integral to array of separate digits: convert 10 -> [1,0]
convert :: Integral a => a -> [a]
convert 0 = []
convert x = convert (x `div` 10) ++ [x `mod` 10]

-- Tests
main :: IO Counts
main =  runTestTT tests

testUnit :: (Eq a, Show a) => String ->  a -> a -> Test
testUnit s x y  = TestCase (assertEqual s x (y) )

tests :: Test
tests = TestList [
 (testUnit "test" 18 (day2 [5195,753,2468])),
 (testUnit "test" 0 (day2 [11,11,11])),
 (testUnit "test" 0 (day2 [1,1,1])),
 (testUnit "test" 4 (day2 [12,21,13]))
 ]
