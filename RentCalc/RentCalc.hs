import System.Exit
import System.Environment

main = do args <- getArgs
          checkArgs args
          printContr (words (args !! 0)) (map read (words (args !! 1))) (words (args !! 2))

checkArgs :: [String] -> IO ()
checkArgs args
    | ((length args) == 0) = do (putStr "./RentCalc 'Name1 Name2 Name3 . . .' 'Rent Expense1 Expense2 . . .' 'RentPayer Expense1Payer Expense2Payer . . .'")
                                exitFailure
    | otherwise = putStr ""

printPretty :: [([Char], Float)] -> [Char] -> IO ()
printPretty [] s = putStr (s ++ "\n")
printPretty ((person,amount):xs) s = printPretty xs (s ++ "\n" ++ (person ++ ": $" ++ (show amount)))

f :: [Char] -> Float -> [([Char],Float)] -> Float -> Float
f _ amount [] _ = amount
f name amount ((pn,am):xs) numberOfOthers
    | pn == "All" = (f (name) (amount) (xs) numberOfOthers)
    | name == pn  = (f (name) (amount - (am * numberOfOthers)) (xs) numberOfOthers)
    | otherwise   = (f (name) (amount + am) (xs) numberOfOthers)

adjust :: Float -> (([Char],Float),[([Char],Float)]) -> ([Char],Float)
adjust numberOfOthers (person, allContr) = (fst person, (f (fst person) (snd person) (allContr) numberOfOthers))

printContr :: [[Char]] -> [Float] -> [[Char]] -> IO ()
printContr people bills whoPaid = do let total          = (bills !! 0)
                                     let pNum           = (fromIntegral (length people)) :: Float
                                     let divide         = (map (/pNum) bills)
                                     let contributions  = (zip people         (replicate (length people)         (total/pNum)))
                                     let preAdjustment  = (zip contributions  (replicate (length contributions)  (zip whoPaid divide)))
                                     let final          = (map (adjust (pNum - 1)) preAdjustment)
                                     printPretty final ""
                                     putStr ("Total: $" ++ (show (sum (map snd final))) ++ "\n")
                                     exitSuccess
