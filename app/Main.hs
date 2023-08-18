{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Char
import System.Console.Haskeline
import System.Environment
import Numeric

type MachineCode = String
data LookUp = LookUp String String
data Variable = Variable Char Int

showBinary :: Int -> String
showBinary int = showIntAtBase 2 intToDigit int ""

replaceChars :: String -> Char -> String -> String
replaceChars [] _ _ = []
replaceChars (c:cs) targetChar replacements
    | c == targetChar = head replacements : replaceChars cs targetChar (tail replacements)
    | otherwise = c : replaceChars cs targetChar replacements

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
    | c == x = 1 + countChar c xs
    | otherwise = countChar c xs

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> getFile filePath
        _ -> interactiveMode

interactiveMode :: IO ()
interactiveMode =
    runInputT defaultSettings loop
    where
      loop :: InputT IO ()
      loop = do
          minput <- getInputLine "> "
          case minput of
              Nothing -> return ()
              Just "quit" -> return ()
              Just "exit" -> return ()
              Just input -> do
                  liftIO $ putStrLn $ parseLine input
                  loop

getFile :: String -> IO ()
getFile filePath = do
    contents <- readFile filePath
    putStrLn $ concatMap parseLine (lines contents)

parseLine :: String -> MachineCode
parseLine line
    | null line = ""
    | otherwise =
        let wordsArr = words $ map toLower line
        in 
            case head wordsArr of
                "add" -> parseOperands line [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,%i" "0001nnnniiiiiiii"]
                "or" -> parseOperands line [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,%i" "0011nnnniiiiiiii"]
                _ -> ""

parseOperands :: String -> [LookUp] -> MachineCode
parseOperands line lookUps =
    let cleanLine = filter (/= ' ') . concat . tail $ words line
        matchingLookUps = [(result, vars) | LookUp prefix result <- lookUps, let (matches, vars) = matchesPrefix prefix cleanLine, matches]
    in case matchingLookUps of
        [] -> ""
        ((result, vars):_) -> processVariables result vars

processVariables :: String -> [Variable] -> MachineCode
processVariables result [] = result
processVariables result (Variable char intValue : vars) =
    let binary = showBinary intValue
        len = countChar char result
    in
        case compare (length binary) len of
            GT -> ""
            EQ -> processVariables (replaceChars result char binary) vars
            LT -> processVariables (replaceChars result char (replicate (len - length binary) '0' ++ binary)) vars

matchesPrefix :: String -> String -> (Bool, [Variable])
matchesPrefix [] [] = (True, [])
matchesPrefix ('%':holder:restPrefix) restLine =
    case span isDigit restLine of
        ([], _) -> (False, [])
        (digits, remaining) ->
            let varValue = read digits
                variable = Variable holder varValue
                (success, variables) = matchesPrefix restPrefix remaining
            in
                (success, variable : variables)
matchesPrefix (x:xs) (y:ys) =
    if x == y then matchesPrefix xs ys else (False, [])
matchesPrefix _ _ = (False, [])