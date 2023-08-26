{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Binary
import Data.List (partition)
import qualified Data.ByteString as B (writeFile)
import System.Environment (getArgs)
import Data.Char (toLower, isDigit, isHexDigit)

type MachineCode = String

data Result = Error String Int | MachineCode String
data LookUp = LookUp String String
data Variable = Variable Char Int

instance Show Result where
    show (Error message line) = "Error in line " ++ show line ++ ": " ++ message
    show (MachineCode code) = code

instance Show Variable where
    show (Variable char int) = show char ++ show int

negateVariable :: Variable -> Variable
negateVariable (Variable c i) = Variable c (-i)

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
        0 -> B.writeFile outputPath $ binStringToByteString $ concatMap show machineCodes
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
                "add" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,%i" "0001nnnniiiiiiii"]
                "addc" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0110", LookUp "r%n,%i" "0110nnnniiiiiiii"]
                "or" -> parseOperands line lineNumber [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,%i" "0011nnnniiiiiiii"]
                _ -> Error ("No such mnemonic: '" ++ mnemonic ++ "'") lineNumber

parseOperands :: String -> Int -> [LookUp] -> Result
parseOperands line lineNumber lookUps =
    let withoutMnemonic = tail $ words line
        cleanLine = concat withoutMnemonic
        varsWithResult = map (lookUpToVariable cleanLine) lookUps
        varWithResult = filter variableSuccess varsWithResult
    in case length varWithResult of
        0 -> Error ("Couldn't parse operands: '" ++ unwords withoutMnemonic ++ "' for '" ++ head (words line) ++ "'") lineNumber
        1 -> case head varWithResult of
                Nothing -> error "not possible if done correct"
                Just (vars, result) -> processVars result lineNumber vars
        _ -> error "not possible if done correct"

lookUpToVariable :: String -> LookUp -> Maybe ([Variable], String)
lookUpToVariable line (LookUp pattern' result) =
    let varArray = matchLookups pattern' line [] in
        case varArray of
            Nothing -> Nothing
            Just arr -> Just (arr, result)

matchLookups :: String -> String -> [Variable] -> Maybe [Variable]
matchLookups [] [] vars = Just vars
matchLookups ('%':holder:rest) ('-':restLine) vars =
    let variable = consumeVariable holder restLine isDigit read in
        case variable of
            Nothing -> Nothing
            Just (var, restLine') -> matchLookups rest restLine' (negVar:vars)
                where negVar = negateVariable var
matchLookups ('%':holder:rest) ('+':restLine) vars =
    let variable = consumeVariable holder restLine isDigit read in
        case variable of
            Nothing -> Nothing
            Just (var, restLine') -> matchLookups rest restLine' (negVar:vars)
                where negVar = negateVariable var
matchLookups ('%':holder:rest) ('#':restLine) vars =
    let variable = consumeVariable holder restLine isHexDigit readHex' in
        case variable of
            Nothing -> Nothing
            Just (var, lineLeft) -> matchLookups rest lineLeft (var:vars)
matchLookups ('%':holder:rest) ('%':restLine) vars =
    let variable = consumeVariable holder restLine isBit binToDec in
        case variable of
            Nothing -> Nothing
            Just (var, lineLeft) -> matchLookups rest lineLeft (var:vars)
matchLookups ('%':holder:rest) line vars =
    let variable = consumeVariable holder line isDigit read in
        case variable of
            Nothing -> Nothing
            Just (var, restLine') -> matchLookups rest restLine' (negVar:vars)
                where negVar = negateVariable var
matchLookups (x:xs) (y:ys) vars =
    if x == y then matchLookups xs ys vars else Nothing
matchLookups _ _ _ = Nothing

consumeVariable :: Char -> String -> (Char -> Bool) -> (String -> Int) -> Maybe (Variable, String)
consumeVariable character line isFunc readFunc =
    case span isFunc line of
        ([], _) -> Nothing
        (digits, remaining) ->
            let varValue = readFunc digits in
                Just (Variable character varValue, remaining)

variableSuccess :: Maybe ([Variable], String) -> Bool
variableSuccess input =
    case input of
        Just (_, _) -> True
        _ -> False

processVars :: String -> Int -> [Variable] -> Result
processVars result _ [] = MachineCode result
processVars result lineNumber (Variable char intValue : vars) =
    let availableLen = countChar char result
        bin = showBinary intValue availableLen
    in case bin of
        Left err -> Error err lineNumber
        Right machineCode -> processVars (replaceChars result char machineCode) lineNumber vars

showBinary :: Int -> Int -> Either String MachineCode
showBinary int availableLen
    | nBins > availableLen = Left $ "Couldn't fit " ++ binString bins ++ " into " ++ show availableLen ++ " bits of space."
    | int >= 0 = Right $ binString expandedBins
    | otherwise = Right $ binString $ twosComplement expandedBins
        where
            bins = binary (abs int)
            nBins = length bins
            expandedBins = replicate (availableLen - length bins) 0 ++ bins