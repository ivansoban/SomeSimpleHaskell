module Main where

import Data.Maybe
import Data.List.Split
import Data.DateTime
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Control.Exception
import System.Posix.Process
import System.Posix.Types

instructions = "Usage: todo -(n|v)\n\
               \       -n -> Opens editor to manage todo list\n\
               \       -v -> View todo listm due dates, and descriptions"

defaultFile = "# Default TODO File:\n\
              \# Format: item_name - due_date - description\n\
              \# TODO_Item - 12/1/2014 - Item description"

diffTime :: String -> DateTime -> Integer
diffTime due cur = (diffMinutes (fromMaybe cur ((parseDateTime "%m/%d/%Y") due)) cur)

prettyPrint :: DateTime -> [String] -> String
prettyPrint curTime (name : (dueDate : (desc : _)))
    | (diffTime dueDate curTime) < 0
        = "*** OverDue:                " ++ name ++ " - " ++ dueDate ++ " - " ++ desc
    | (diffTime dueDate curTime) < (60 * 24)
        = "*** Less then 24 hours for: " ++ name ++ " - " ++ dueDate ++ " - " ++ desc
    | otherwise = (take 28 (repeat ' ')) ++ name ++ " - " ++ dueDate ++ " - " ++ desc

readTODO :: String -> DateTime -> String
readTODO todoContents time = (foldr (\l r -> l ++ "\n" ++ r) ""
                                 (map (prettyPrint time)
                                     (map (splitOn "|")
                                         (map unwords
                                             (filter (\l -> ((l !! 0) !! 0) /= '#')
                                                 (map words
                                                     (lines todoContents)))))))


main = do args <- getArgs
          curTime <- getCurrentTime
          f <- getEnv "HOME"
          e <- getEnv "EDITOR"

          let todoFile = (f ++ "/.todo")
          fileExists <- doesFileExist todoFile
          when (not fileExists) $  writeFile todoFile defaultFile
          todo <- readFile todoFile
          case (args !! 0) of "-v" -> putStr (readTODO todo curTime)
                              "-n" -> do exec <- try (executeFile e True [todoFile] Nothing) :: IO (Either SomeException a)
                                         case exec of Left ex   -> putStrLn $ "Caught exception: " ++ show ex ++ ". Please check your $EDITOR variable."
                                                      Right e   -> do pid <- forkProcess e
                                                                      status <- getProcessStatus True True pid
                                                                      case status of Nothing -> error "Error waiting for editor."
                                                                                     Just status -> print status
                              "--help" -> putStr instructions
                              otherwise -> do putStr ("Incorrect flag '" ++ (args !! 0) ++ "'\n")
                                              putStr instructions
