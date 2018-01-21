-- 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
-- 1111 produces 4 because each digit (all 1) matches the next.
-- 1234 produces 0 because no digit matches the next.
-- 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

import Test.HUnit

-- main 
day1 :: (Integral a, Eq a) => a -> a
day1 x = headSum (convert x)

-- converts Integral to array
convert :: Integral a => a -> [a]
convert 0 = []
convert x = convert (x `div` 10) ++ [x `mod` 10]

-- checking the first and last element of an array
headSum :: (Num a, Eq a) => [a] -> a
headSum [] = 0
headSum [x] = x 
headSum (x:xs) = if x == last xs then x + innerSum 0 (x:xs)
                                 else innerSum 0 (x:xs)

-- checking the middle of an array 
innerSum :: (Num a, Eq a) => a -> [a] -> a
innerSum sum [] = sum
innerSum sum [x] = sum
innerSum sum (x:xs) = if x == head xs then innerSum (sum + x) xs 
                                  else innerSum (sum) xs

-- Tests
main :: IO Counts
main =  runTestTT tests

testUnit :: (Eq a, Show a) => String ->  a -> a -> Test
testUnit s x y  = TestCase (assertEqual s x (y) )

tests :: Test
tests = TestList [
 (testUnit "test" 1 (day1 1123)),
 (testUnit "test" 1 (day1 1)), 
 (testUnit "test" 0 (day1 12)), 
 (testUnit "test" 4 (day1 22)), 
 (testUnit "test" 3 (day1 1122)), 
 (testUnit "test" 4 (day1 1111)), 
 (testUnit "test" 1 (day1 0110)), 
 (testUnit "test" 1 (day1 0112)),
 (testUnit "test" 2 (day1 2111))
 ]
