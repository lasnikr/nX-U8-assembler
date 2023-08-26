{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Binary
import Data.List (partition)
import qualified Data.ByteString as B (writeFile)
import System.Environment (getArgs)
import Data.Char (toLower, isDigit, isHexDigit, isSpace)

type MachineCodeStr = String
type ErrorStr = String

data Result = Error String Int | MachineCode String
data LookUp = LookUp String String
data Variable = Variable Char Int

instance Show Result where
    show (Error message line) = "Error in line " ++ show line ++ ": " ++ message ++ "."
    show (MachineCode code) = code

instance Show Variable where
    show (Variable char int) = show char ++ show int

negateVariable :: Variable -> Variable
negateVariable (Variable c i) = Variable c (-i)

readHex' :: String -> Int
readHex' hex = read $ "0x" ++ hex

replaceChars :: String -> Char -> String -> Maybe MachineCodeStr
replaceChars [] _ _ = Just []
replaceChars (num:'>':c:cs) targetChar (r:rs)
    | c == targetChar && num == r = (num:) <$> replaceChars cs targetChar rs
    | c == targetChar = Nothing
    | otherwise = ((num:) <$> ('>':)) . (c:) <$> replaceChars cs targetChar (r:rs)
replaceChars (c:cs) targetChar replacements
    | c == targetChar = (head replacements:) <$> replaceChars cs targetChar (tail replacements)
    | otherwise = (c:) <$> replaceChars cs targetChar replacements

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
    | c == x = 1 + countChar c xs
    | otherwise = countChar c xs

variableSuccess :: Maybe ([Variable], String) -> Bool
variableSuccess input =
    case input of
        Just (_, _) -> True
        _ -> False

isError :: Result -> Bool
isError (Error _ _) = True
isError _ = False

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

parseLine :: String -> Int -> Result
parseLine line lineNumber
    | all isSpace line = MachineCode ""
    | otherwise =
        let wordsArr = words $ map toLower line
            mnemonic = head wordsArr
            lookupTable = case mnemonic of
                "add" -> [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,%i" "0001nnnniiiiiiii", LookUp "er%n,er%m" "1111nnn0>nmmm0>m0110", LookUp "er%n,%i" "1110nnn0>n1iiiiiii"]
                "addc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0110", LookUp "r%n,%i" "0110nnnniiiiiiii"]
                "and" -> [LookUp "r%n,r%m" "1000nnnnmmmm0010", LookUp "r%n,%i" "0010nnnniiiiiiii"]
                "cmp" -> [LookUp "r%n,r%m" "1000nnnnmmmm0111", LookUp "r%n,%i" "0111nnnniiiiiiii"]
                "cpmc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0101", LookUp "r%n,%i" "0101nnnniiiiiiii"]
                "mov" -> [LookUp "er%n, er%m" "1111nnn0mmm00101", LookUp "er%n,%i" "1110nnn00iiiiiii"]
                "or" -> [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,%i" "0011nnnniiiiiiii"]
                _ -> []
            parseResult = parseOperands line lookupTable
        in
            case parseResult of
                Left err -> Error err lineNumber
                Right code -> MachineCode code

parseOperands :: String -> [LookUp] -> Either ErrorStr MachineCodeStr
parseOperands line [] = Left $ "Mnemonic '" ++ head (words line) ++ "' not found"
parseOperands line lookUps =
    case varWithResult of
        [Just (vars, result)] -> processVars result (reverse vars)
        _ -> Left errorMessage
    where
        withoutMnemonic = tail $ words line
        cleanLine = concat withoutMnemonic
        varsWithResult = map (lookUpToVariable cleanLine) lookUps
        varWithResult = filter variableSuccess varsWithResult
        errorMessage =
            let mnemonic = head (words line)
                operands = unwords withoutMnemonic
            in "Couldn't parse operands: '" ++ operands ++ "' for '" ++ mnemonic ++ "'"

processVars :: String -> [Variable] -> Either ErrorStr MachineCodeStr
processVars result [] = Right result
processVars result (Variable char intValue : vars) =
    let availableLen = countChar char result
        bin = showBinary intValue availableLen
    in case bin of
        Left err -> Left err
        Right machineCode ->
            case replaceChars result char machineCode of
                Just newResult -> processVars newResult vars
                Nothing -> Left $ "'" ++ machineCode ++ "' doesnt match with '" ++ result ++ "' for " ++ show char


lookUpToVariable :: String -> LookUp -> Maybe ([Variable], String)
lookUpToVariable line (LookUp pattern' result) =
    let varArray = matchLookups pattern' line [] in
        case varArray of
            Nothing -> Nothing
            Just arr -> Just (arr, result)

matchLookups :: String -> String -> [Variable] -> Maybe [Variable]
matchLookups [] [] vars = Just vars
matchLookups ('%':holder:rest) line vars =
    let consume = case line of
            ('-':restLine) -> consumeVariable holder restLine isDigit read
            ('+':restLine) -> consumeVariable holder restLine isDigit read
            ('#':restLine) -> consumeVariable holder restLine isHexDigit readHex'
            ('%':restLine) -> consumeVariable holder restLine isBit binToDec
            _ -> consumeVariable holder line isDigit read
    in
    case consume of
        Nothing -> Nothing
        Just (var, restLine') -> matchLookups rest restLine' (adjust var:vars)
            where adjust = case line of
                    ('-':_) -> negateVariable
                    _ -> id
matchLookups (x:xs) (y:ys) vars
    | x == y = matchLookups xs ys vars
    | otherwise = Nothing
matchLookups _ _ _ = Nothing

consumeVariable :: Char -> String -> (Char -> Bool) -> (String -> Int) -> Maybe (Variable, String)
consumeVariable character line isFunc readFunc =
    case span isFunc line of
        ([], _) -> Nothing
        (digits, remaining) ->
            let varValue = readFunc digits in
                Just (Variable character varValue, remaining)

showBinary :: Int -> Int -> Either ErrorStr MachineCodeStr
showBinary int availableLen
    | nBins > availableLen = Left $ "Couldn't fit binary " ++ binString bins ++ " (" ++ show nBins ++ ") into " ++ show availableLen ++ " bits of space."
    | int >= 0 = Right $ binString expandedBins
    | otherwise = Right $ binString $ twosComplement expandedBins
        where
            bins = binary (abs int)
            nBins = length bins
            expandedBins = replicate (availableLen - length bins) 0 ++ bins