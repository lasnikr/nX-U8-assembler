{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Char
import System.Console.Haskeline
import System.Environment
import Numeric
import qualified Data.ByteString as B
import Data.List

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

binaryChunkToByte :: MachineCode -> Int
binaryChunkToByte st = foldl' (\x acc -> x * 2 + acc ) 0 (map digitToInt st)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

littleEndian :: [MachineCode] -> [MachineCode]
littleEndian [] = []
littleEndian [x] = [x]
littleEndian (x:y:xs) = y : x : littleEndian xs

binaryStringToByteString :: MachineCode -> B.ByteString
binaryStringToByteString binary =
    B.pack $ map (fromIntegral . binaryChunkToByte) $ littleEndian $ chunksOf 8 binary

insertUnderscores :: MachineCode -> MachineCode
insertUnderscores [] = []
insertUnderscores xs = let (chunk, rest) = splitAt 4 xs
                        in
                            if null rest then chunk
                            else chunk ++ "_" ++ insertUnderscores rest

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inputPath:outputPath:_) -> getFile inputPath outputPath
        _ -> do
            putStrLn "Entering interactive mode."
            putStrLn "To use the assembler on a file, run 'nxasm [inputPath] [outputPath]'"
            interactiveMode

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
                    liftIO $ putStrLn $ insertUnderscores $ parseLine input
                    loop

getFile :: FilePath -> FilePath  -> IO ()
getFile inputPath outputPath = do
    contents <- readFile inputPath
    B.writeFile outputPath $ binaryStringToByteString $ concatMap parseLine (lines contents)

parseLine :: String -> MachineCode
parseLine line
    | null line = ""
    | otherwise =
        let wordsArr = words $ map toLower line
            mnemonic = head wordsArr
        in
            case mnemonic of
                "add" -> parseOperands line [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,%i" "0001nnnniiiiiiii"]
                "or" -> parseOperands line [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,%i" "0011nnnniiiiiiii"]
                _ -> error $ "No such mnemonic: '" ++ mnemonic ++ "'"

parseOperands :: String -> [LookUp] -> MachineCode
parseOperands line lookUps =
    let withoutMnemonic = concat . tail $ words line
        cleanLine = filter (/= ' ') withoutMnemonic
        matchingLookUps = [(result, vars) | LookUp prefix result <- lookUps, let (matches, vars) = matchLookups prefix cleanLine, matches]
    in case matchingLookUps of
        [] -> error $ "Couldn't parse operands: '" ++ withoutMnemonic ++ "' for '" ++ head (words line) ++ "'"
        ((result, vars):_) -> processVariables result vars

processVariables :: String -> [Variable] -> MachineCode
processVariables result [] = result
processVariables result (Variable char intValue : vars) =
    let binary = showBinary intValue
        availableLength = countChar char result
        inputLength = length binary
    in
        case compare inputLength availableLength of
            GT -> error $ "Number not in valid range: " ++ show inputLength ++ " > " ++ show availableLength
            EQ -> processVariables (replaceChars result char binary) vars
            LT -> processVariables (replaceChars result char (replicate (availableLength - inputLength) '0' ++ binary)) vars

matchLookups :: String -> String -> (Bool, [Variable])
matchLookups [] [] = (True, [])
matchLookups ('%':holder:restPrefix) restLine =
    case span isDigit restLine of
        ([], _) -> (False, [])
        (digits, remaining) ->
            let varValue = read digits
                variable = Variable holder varValue
                (success, variables) = matchLookups restPrefix remaining
            in
                (success, variable : variables)
matchLookups (x:xs) (y:ys) =
    if x == y then matchLookups xs ys else (False, [])
matchLookups _ _ = (False, [])