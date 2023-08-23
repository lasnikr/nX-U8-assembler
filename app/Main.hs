{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Data.Char
import System.Environment
import Numeric
import qualified Data.ByteString as B
import Data.List

type MachineCode = String

data Result = Error String Int | MachineCode String
data LookUp = LookUp String String
data Variable = Variable Char Int

instance Show Result where
    show (Error message line) = "Error in line " ++ show line ++ ": " ++ message
    show (MachineCode code) = code

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
    let results = zipWith parseLine (lines contents) [1 ..]
        resultSplit = partition isError results
        errors = fst resultSplit
        machineCodes = snd resultSplit
    case length errors of
        0 -> B.writeFile outputPath $ binaryStringToByteString $ concatMap show machineCodes
        _ -> mapM_ print errors

isError :: Result -> Bool
isError (Error _ _) = True
isError _ = False

parseLine :: String -> Int -> Result
parseLine line lineNumber
    | null line = MachineCode ""
    | otherwise =
        let wordsArr = words $ map toLower line
            mnemonic = head wordsArr
        in
            case mnemonic of
                "add" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,#i" "0001nnnniiiiiiii"]
                "addc" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000_nnnn_mmmm_0110", LookUp "r%n,#i" "0110_nnnn_iiii_iiii"]
                "or" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,#i" "0011nnnniiiiiiii"]
                _ -> Error ("No such mnemonic: '" ++ mnemonic ++ "'") lineNumber

parseOperands :: String -> Int -> [LookUp] -> Result
parseOperands line lineNumber lookUps =
    let withoutMnemonic = tail $ words line
        cleanLine = concat withoutMnemonic
        matchingLookUps = [(result, vars) | LookUp lookupEntry result <- lookUps, let (matches, vars) = matchLookups lookupEntry cleanLine, matches]
    in case matchingLookUps of
        [] -> Error ("Couldn't parse operands: '" ++ unwords withoutMnemonic ++ "' for '" ++ head (words line) ++ "'") lineNumber
        ((result, vars):_) -> processVariables result lineNumber vars

processVariables :: String -> Int -> [Variable] -> Result
processVariables result _ [] = MachineCode result
processVariables result lineNumber (Variable char intValue : vars) =
    let binary = showBinary intValue
        availableLength = countChar char result
        inputLength = length binary
    in
        case compare inputLength availableLength of
            GT -> Error ("Number not in valid range: " ++ show intValue ++ " > " ++ show (2 ^ availableLength - 1 :: Int)) lineNumber
            EQ -> processVariables (replaceChars result char binary) lineNumber vars
            LT -> processVariables (replaceChars result char (replicate (availableLength - inputLength) '0' ++ binary)) lineNumber vars

matchLookups :: String -> String -> (Bool, [Variable])
matchLookups [] [] = (True, [])
matchLookups ('%':holder:rest) ('#':restLine) =
    consumeVariable holder restLine rest isHexDigit readHex'
matchLookups ('%':holder:rest) restLine =
    consumeVariable holder restLine rest isDigit read
matchLookups (x:xs) (y:ys) =
    if x == y then matchLookups xs ys else (False, [])
matchLookups _ _ = (False, [])

consumeVariable :: Char -> String -> String -> (Char -> Bool) -> (String -> Int) -> (Bool, [Variable])
consumeVariable character line rest isFunc readFunc =
    case span isFunc line of
        ([], _) -> (False, [])
        (digits, remaining) ->
            let varValue = readFunc digits
                variable = Variable character varValue
                (success, variables) = matchLookups rest remaining
            in
                (success, variable : variables)

readHex' :: String -> Int
readHex' hex = read $ "0x" ++ hex