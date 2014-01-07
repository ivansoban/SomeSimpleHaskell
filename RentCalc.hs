import System.Exit
import System.Environment

main = do args <- getArgs
          checkArgs args
          printContr (words (args !! 0)) (map read (words (args !! 1))) (words (args !! 2))
          -- printContr ["Ivan","Katy","Tyler"] [1312.01,154.94,84.74,25.00] ["All", "Ivan","Ivan","Tyler"]

checkArgs :: [String] -> IO ()
checkArgs args
    | ((length args) == 0) = do (putStr "./rentmoney 'Name1 Name2 Name3 . . .' 'Rent Expense1 Expense2 . . .' 'RentPayer Expense1Payer Expense2Payer . . .'")
                                exitFailure
    | otherwise = putStr ""

printPretty :: [([Char], Float)] -> [Char] -> IO ()
printPretty [] s = putStr (s ++ "\n")
printPretty ((person,amount):xs) s = printPretty xs (s ++ "\n" ++ (person ++ ": $" ++ (show amount)))

f :: [Char] -> Float -> [([Char],Float)] -> Float
f _ amount [] = amount
f name amount ((pn,am):xs)
    | pn == "All" = (f (name) (amount) (xs))
    | name == pn = (f (name) (amount - (am * 2)) (xs))
    | otherwise = (f (name) (amount + am) (xs))

adjust :: (([Char],Float),[([Char],Float)]) -> ([Char],Float)
adjust (person,allContr) = (fst person, (f (fst person) (snd person) (allContr)))

printContr :: [[Char]] -> [Float] -> [[Char]] -> IO ()
printContr people bills whoPaid = do let total = (bills !! 0)
                                     let third = map (/3) bills
                                     let contr = (zip people (replicate (length people) (total/3)))
                                     let acont = zip contr (replicate (length contr) (zip whoPaid third))
                                     let final = (map adjust acont)
                                     printPretty final ""
                                     putStr ("Total: $" ++ (show (sum (map snd final))) ++ "\n")
                                     exitSuccess
