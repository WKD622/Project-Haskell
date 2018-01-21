-- aa bb cc dd ee is valid.
-- aa bb cc dd aa is not valid - the word aa appears more than once.
-- aa bb cc dd aaa is valid - aa and aaa count as different words.

import Test.HUnit

-- main 
day4 :: [[Char]] -> Bool
day4 [] = True 
day4 [x] = True 
day4 (x:xs) = if chooseListToCompare x xs then day4 xs
                                        else False

-- compares Strings in list till everypair is checked. There can't be two or more same strings.
chooseListToCompare :: [Char] -> [[Char]] -> Bool
chooseListToCompare y [] = True
chooseListToCompare y (x:xs) = if not(isSame y x) then chooseListToCompare y xs    
                                    else False

-- checks if two chars are same or not
isSame :: [Char] -> [Char] -> Bool
isSame a b = if a == b then True 
                        else False

-- Tests
main :: IO Counts
main =  runTestTT tests

testUnit :: (Eq a, Show a) => String ->  a -> a -> Test
testUnit s x y  = TestCase (assertEqual s x (y) )

tests :: Test
tests = TestList [
 (testUnit "test" True (day4 ["aa","bb","cc","dd","ee"])),
 (testUnit "test" False (day4 ["aa","bb","cc","dd","aa"])),
 (testUnit "test" True (day4 ["aa","bb","cc","dd","aaa"]))
 ]
