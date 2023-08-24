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
data Variable = Variable Char String

instance Show Result where
    show (Error message line) = "Error in line " ++ show line ++ ": " ++ message
    show (MachineCode code) = code

showBinary :: Int -> String
showBinary int
    | int >= 0 = showIntAtBase 2 intToDigit int ""
    | otherwise = showIntAtBase 2 intToDigit (abs int) ""

-- two'sComplement :: String -> String
-- two'sComplement 

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

readHex' :: String -> Int
readHex' hex = read $ "0x" ++ hex

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

        vars = map (lookUpToVariable cleanLine) lookUps
        var = filter variableSuccess vars
    in case var of
        0 -> Error "" lineNumber
        1 -> processVariable result lineNumber vars
        _ -> error "not possible if done correct"

lookUpToVariable :: String -> LookUp -> Maybe ([Variable], String)
lookUpToVariable line (LookUp pattern' result) = 
    let varArray = matchLookups pattern' line [] in
        case varArray of 
            Nothing -> Nothing 
            Just arr -> Just (arr, result)

variableSuccess :: Maybe ([Variable], String) -> Bool 
variableSuccess input =
    case input of
        Just ([], _) -> True
        Just ([_], _) -> True
        _ -> False

matchLookups :: String -> String -> [Variable] -> Maybe [Variable]
matchLookups [] [] vars = Just vars
matchLookups ('%':holder:rest) ('-':restLine) vars =
    let variable = consumeVariable holder restLine isDigit read in
        case variable of 
            Nothing -> Nothing 
            Just (var, restLine') -> matchLookups rest restLine' (var:vars)
matchLookups ('%':holder:rest) ('#':restLine) vars =
    let variable = consumeVariable holder restLine isHexDigit readHex' in
        case variable of 
            Nothing -> Nothing 
            Just (var, lineLeft) -> matchLookups rest lineLeft (var:vars)            
matchLookups (x:xs) (y:ys) vars =
    if x == y then matchLookups xs ys vars else Nothing
matchLookups _ _ _ = Nothing

processVariable :: String -> Int -> Variable -> Result
processVariable result _ [] = MachineCode result
processVariable result lineNumber (Variable char intValue : vars) =
    let binary = showBinary intValue
        availableLength = countChar char result
        inputLength = length binary
    in
        case compare inputLength availableLength of
            GT -> Error ("Number not in valid range: " ++ show intValue ++ " > " ++ show (2 ^ availableLength - 1 :: Int)) lineNumber
            EQ -> processVariables (replaceChars result char binary) lineNumber vars
            LT -> processVariables (replaceChars result char (replicate (availableLength - inputLength) '0' ++ binary)) lineNumber vars


consumeVariable :: Char -> String -> (Char -> Bool) -> (String -> Int) -> Maybe (Variable, String)
consumeVariable character line isFunc readFunc =
    case span isFunc line of
        ([], _) -> Nothing
        (digits, remaining) ->
            let varValue = readFunc digits in
                Just (Variable character (showBinary varValue), remaining)