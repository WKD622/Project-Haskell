main = putStrLn . show $ findManhathan 325489

findManhathan x = abs (n - (x - c n) `mod` (2 * n)) + n
    where n = floor (0.5 * (sqrt(fromIntegral x) + 1))
          c n = 1 + 4 * n * (n - 1)