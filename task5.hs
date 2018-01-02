
day5 :: (Num a, Ord a)  => [a] -> a
day5 x = jump 0 1 x

check :: (Num a, Ord a) => a-> [a] -> Bool
check l x = l > len x
    where len [] = 0
          len (x:xs) = 1 + (len xs)

takeElement :: (Num a, Eq a) => a -> [a] -> a
takeElement _ [] = 0
takeElement a (x:xs) = if a == 1 then x
                                 else takeElement (a-1) xs 

jump :: (Num a, Ord a) => a -> a -> [a] -> a 
jump c a [] = c 
jump c a x = if check (a + takeElement a x) x || a<1 then c + 1
                          else jump (c+1) (a + add) (increment a x)
                    where add = takeElement a x

increment :: (Num a, Eq a) => a -> [a] -> [a] 
increment _ [] = []
increment a (x:xs) = if a == 1 then (x+1):increment (a-1) xs
                                else x:increment (a-1) xs