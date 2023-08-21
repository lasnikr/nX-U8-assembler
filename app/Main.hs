{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Data.Char
import System.Environment
import Numeric
import qualified Data.ByteString as B
import Data.List
import Control.Monad (zipWithM)

type MachineCode = String

data Error = Error String Int
data LookUp = LookUp String String
data Variable = Variable Char Int

instance Show Error where
    show (Error message line) = "Error in line " ++ show line ++ ": " ++ message

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

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inputPath:outputPath:_) -> getFile inputPath outputPath
        _ -> putStrLn "To use the assembler on a file, run 'nxasm [inputPath] [outputPath]'."

getFile :: FilePath -> FilePath -> IO ()
getFile inputPath outputPath = do
    contents <- readFile inputPath
    case zipWithM parseLine (lines contents) [1 ..] of
        Right machineCodes -> B.writeFile outputPath $ binaryStringToByteString $ concat machineCodes
        Left err -> print err

parseLine :: String -> Int -> Either Error MachineCode
parseLine line lineNumber
    | null line = Right ""
    | otherwise =
        let wordsArr = words $ map toLower line
            mnemonic = head wordsArr
        in
            case mnemonic of
                "add" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,%i" "0001nnnniiiiiiii"]
                "or" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,%i" "0011nnnniiiiiiii"]
                _ -> Left $ Error ("No such mnemonic: '" ++ mnemonic ++ "'") lineNumber

parseOperands :: String -> Int -> [LookUp] -> Either Error MachineCode
parseOperands line lineNumber lookUps =
    let withoutMnemonic = tail $ words line
        cleanLine = concat withoutMnemonic
        matchingLookUps = [(result, vars) | LookUp prefix result <- lookUps, let (matches, vars) = matchLookups prefix cleanLine, matches]
    in case matchingLookUps of
        [] -> Left $ Error ("Couldn't parse operands: '" ++ unwords withoutMnemonic ++ "' for '" ++ head (words line) ++ "'") lineNumber
        ((result, vars):_) -> processVariables result lineNumber vars

processVariables :: String -> Int -> [Variable] -> Either Error MachineCode
processVariables result _ [] = Right result
processVariables result lineNumber (Variable char intValue : vars) =
    let binary = showBinary intValue
        availableLength = countChar char result
        inputLength = length binary
    in
        case compare inputLength availableLength of
            GT -> Left $ Error ("Number not in valid range: " ++ show intValue ++ " > " ++ show (2 ^ availableLength - 1 :: Int)) lineNumber
            EQ -> processVariables (replaceChars result char binary) lineNumber vars
            LT -> processVariables (replaceChars result char (replicate (availableLength - inputLength) '0' ++ binary)) lineNumber vars

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