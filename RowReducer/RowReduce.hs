import Text.Printf

main = do stdin <- getContents
          let fm = (fphase [[]]
                       (map (map read)
                           (map words
                               (lines stdin))))
          printmatrix (reverse
                          (fphase [[]]
                              (reverse
                                  fm)))

printrow :: [Float] -> IO ()
printrow [] = printf "\n"
printrow (x:xs) = do printf "%5f " x
                     printrow xs

printmatrix :: [[Float]] -> IO ()
printmatrix [] = printf ""
printmatrix (x:xs) = do printrow x
                        printmatrix xs

ldindex :: [Float] -> Int -> Int
ldindex [] _ = -1
ldindex (e:xs) i
    | e == 0 = ldindex xs (i + 1)
    | e /= 0  = i

ero :: [Float] -> [Float] -> [Float]
ero row1 row2 = let scale = (row2 !! (ldindex row1 0)) * (-1)
                in  (zipWith (+) (map (* scale) row1) row2)

fphase :: [[Float]] -> [[Float]] -> [[Float]]
fphase (e:top) [] = top
fphase top (row:xs)
    | lindex == -1 = fphase top xs
    | ((row !! lindex) /= 1) = fphase top ((map (/ (row !! lindex)) row):xs)
    | otherwise = fphase (top ++ [row]) (map (ero row) xs)
    where lindex = (ldindex row 0)
