
day4 :: [[Char]] -> Bool
day4 [] = True 
day4 [x] = True 
day4 (x:xs) = if chooseListToCompare x xs then day4 xs
                                        else False

chooseListToCompare :: [Char] -> [[Char]] -> Bool
chooseListToCompare y [] = True
chooseListToCompare y (x:xs) = if not(isSame y x) then chooseListToCompare y xs    
                                    else False

isSame :: [Char] -> [Char] -> Bool
isSame a b = if a == b then True 
                        else False




