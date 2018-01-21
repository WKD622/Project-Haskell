import Test.HUnit

-- basically fires jump
day5 :: (Num a, Ord a)  => [a] -> a
day5 x = jump 0 1 x

--check if number (first arg) is bigger than array length
check :: (Num a, Ord a) => a-> [a] -> Bool
check l x = l > len x
    where len [] = 0
          len (x:xs) = 1 + (len xs)

 -- eg. takeElement n [...] -Takes n-th element of an array (counting from 1, not 0)        
takeElement :: (Num a, Eq a) => a -> [a] -> a
takeElement _ [] = 0
takeElement a (x:xs) = if a == 1 then x
                                 else takeElement (a-1) xs 
 
-- returns number of 'jumps' required to get outof array bounds eg: jump 0 1 [1,1,1,1] -> 4    
--jump c a [...] - c-number of counts, a- begining index 
jump :: (Num a, Ord a) => a -> a -> [a] -> a 
jump c a [] = c 
jump c a x = if check (a + takeElement a x) x || a<1 then c + 1
                          else jump (c+1) (a + add) (increment a x)
                    where add = takeElement a x

-- increments element of chosen index in an array (counting from 1 to n) eg. increment 2 [1,1,1] ->  increment 2 [1,1,1]   
increment :: (Num a, Eq a) => a -> [a] -> [a] 
increment _ [] = []
increment a (x:xs) = if a == 1 then (x+1):increment (a-1) xs
                                else x:increment (a-1) xs

-- Tests
main :: IO Counts
main =  runTestTT tests

testUnit :: (Eq a, Show a) => String ->  a -> a -> Test
testUnit s x y  = TestCase (assertEqual s x (y) )

tests :: Test
tests = TestList [
 (testUnit "test" 5 (day5 [0,3,0,1,-3])),
 (testUnit "test" 5 (day5 [1,1,1,1,1])),
 (testUnit "test" 4 (day5 [1,1,1,2,1])),
 (testUnit "test" 5 (day5 [1,1,-1,2,1]))
 ]

