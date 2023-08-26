{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Binary
import qualified Data.ByteString as B (writeFile)
import Data.Char (isDigit, isHexDigit, isSpace, toLower)
import Data.List (partition)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

type MachineCodeStr = String

type ErrorStr = String

type DSR = String

data Result = Error String Int | MachineCode String

data LookUp = LookUp String String

data Variable = Variable Char Int

instance Show Result where
  show (Error message line) = "Error in line " ++ show line ++ ": " ++ message ++ "."
  show (MachineCode code) = code

instance Show Variable where
  show (Variable char int) = show char ++ show int

negateVariable :: Variable -> Variable
negateVariable (Variable c i) = Variable c (- i)

readHex' :: String -> Int
readHex' hex = read $ "0x" ++ hex

replaceChars :: String -> Char -> String -> Maybe MachineCodeStr
replaceChars [] _ _ = Just []
replaceChars (num : '>' : c : cs) targetChar (r : rs)
  | c == targetChar && num == r = (num :) <$> replaceChars cs targetChar rs
  | c == targetChar = Nothing
  | otherwise = ((num :) <$> ('>' :)) . (c :) <$> replaceChars cs targetChar (r : rs)
replaceChars (c : cs) targetChar replacements
  | c == targetChar = (head replacements :) <$> replaceChars cs targetChar (tail replacements)
  | otherwise = (c :) <$> replaceChars cs targetChar replacements

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x : xs)
  | c == x = 1 + countChar c xs
  | otherwise = countChar c xs

variableSuccess :: Maybe ([Variable], DSR, String) -> Bool
variableSuccess input =
  case input of
    Just (_, _, _) -> True
    _ -> False

isError :: Result -> Bool
isError (Error _ _) = True
isError _ = False

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inputPath : outputPath : _) -> getFile inputPath outputPath
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
    let lowerLine = map toLower line
        wordsArr = words lowerLine
        mnemonic = head wordsArr
        lookupTable = case mnemonic of
          -- Arithmetic Instructions
          "add" -> [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,#i" "0001nnnniiiiiiii", LookUp "er%n,er%m" "1111nnn0>nmmm0>m0110", LookUp "er%n,#i" "1110nnn0>n1iiiiiii"]
          "addc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0110", LookUp "r%n,#i" "0110nnnniiiiiiii"]
          "and" -> [LookUp "r%n,r%m" "1000nnnnmmmm0010", LookUp "r%n,#i" "0010nnnniiiiiiii"]
          "cmp" -> [LookUp "r%n,r%m" "1000nnnnmmmm0111", LookUp "r%n,#i" "0111nnnniiiiiiii", LookUp "er%n,er%m" "1111_nnn0>n_mmm0_0111"]
          "cpmc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0101", LookUp "r%n,#i" "0101nnnniiiiiiii"]
          "mov" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_0000", LookUp "r%n,#i" "0000_nnnn_iiii_iiii", LookUp "er%n, er%m" "1111nnn0>nmmm00101", LookUp "er%n,#i" "1110nnn0>n0iiiiiii"]
          "or" -> [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,#i" "0011nnnniiiiiiii"]
          "xor" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_0100", LookUp "r%n,%i" "0100_nnnn_iiii_iiii"]
          "sub" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1000"]
          "subc" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1001"]
          -- Shift Instructions
          "sll" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1010", LookUp "r%n,%w" "1001_nnnn_0www_1010"]
          "sllc" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1011", LookUp "r%n,%w" "1001_nnnn_0www_1011"]
          "sra" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1110", LookUp "r%n,%w" "1001_nnnn_0www_1110"]
          "srl" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1100", LookUp "r%n,%w" "1001_nnnn_0www_1100"]
          "srlc" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1101", LookUp "r%n,%w" "1001_nnnn_0www_1101"]
          -- Load Instructions
          "l" -> [LookUp "er%n,:[ea]" "1001nnn000110010"]
          -- Store Instructions
          _ -> []
        parseResult = parseOperands lowerLine lookupTable
     in case parseResult of
          Left err -> Error err lineNumber
          Right code -> MachineCode code

parseOperands :: String -> [LookUp] -> Either ErrorStr MachineCodeStr
parseOperands line [] = Left $ "Mnemonic '" ++ head (words line) ++ "' not found"
parseOperands line lookUps =
  case varWithResult of
    [Just (vars, dsr, result)] ->
      case processVars result (reverse vars) of
        Right machineCode -> Right $ dsr ++ machineCode
        Left errorStr -> Left errorStr
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

lookUpToVariable :: String -> LookUp -> Maybe ([Variable], DSR, String)
lookUpToVariable line (LookUp pattern' result) =
  let varArray = matchLookups pattern' line [] ""
   in case varArray of
        Nothing -> Nothing
        Just (arr, dsr) -> Just (arr, dsr, result)

matchLookups :: String -> String -> [Variable] -> DSR -> Maybe ([Variable], DSR)
matchLookups [] [] vars dsr = Just (vars, dsr)
matchLookups (':' : rest) line vars dsr =
    case consumeDSR line of
        Just (restLine, dsr') -> matchLookups rest restLine vars dsr'
        Nothing -> matchLookups rest line vars dsr
matchLookups ('#' : holder : rest) line vars dsr =
  let consume = case line of
        ('-' : restLine) -> consumeVariable holder restLine isDigit read
        ('+' : restLine) -> consumeVariable holder restLine isDigit read
        ('#' : restLine) -> consumeVariable holder restLine isHexDigit readHex'
        ('%' : restLine) -> consumeVariable holder restLine isBit binToDec
        _ -> consumeVariable holder line isDigit read
   in case consume of
        Nothing -> Nothing
        Just (var, lineLeft) -> matchLookups rest lineLeft (adjust var : vars) dsr
          where
            adjust = case line of
              ('-' : _) -> negateVariable
              _ -> id
matchLookups ('%' : holder : rest) line vars dsr =
  let consume = consumeVariable holder line isDigit read
   in case consume of
        Nothing -> Nothing
        Just (var, lineLeft) -> matchLookups rest lineLeft (var : vars) dsr
matchLookups (x : xs) (y : ys) vars dsr
  | x == y = matchLookups xs ys vars dsr
  | otherwise = Nothing
matchLookups _ _ _ _ = Nothing

consumeDSR :: String -> Maybe (String, String)
consumeDSR line =
  case span isDigit line of
    ([], _) ->
      case line of
        ('d' : 's' : 'r' : ':' : r) -> Just (r, "1111111010011111")
        ('r' : r) ->
            case span isDigit r of
                ([], _) -> Nothing
                (nums, ':' : rest) -> Just (rest, dsr)
                    where
                        digits = read nums :: Int
                        bin = case showBinary digits 4 of
                            Left _ -> ""
                            Right code -> code
                        dsr = fromMaybe "" (replaceChars "10010000dddd1111" 'd' bin)
                _ -> Nothing
        _ -> Nothing
    (digits, ':' : rest) -> Just (rest, dsr)
        where
            numDigits = read digits :: Int
            bin = case showBinary numDigits 8 of
                Left _ -> ""
                Right code -> code
            dsr = fromMaybe "" (replaceChars "11100011dddddddd" 'd' bin)
    _ -> Nothing

consumeVariable :: Char -> String -> (Char -> Bool) -> (String -> Int) -> Maybe (Variable, String)
consumeVariable character line isFunc readFunc =
  case span isFunc line of
    ([], _) -> Nothing
    (digits, remaining) ->
      let varValue = readFunc digits
       in Just (Variable character varValue, remaining)

showBinary :: Int -> Int -> Either ErrorStr MachineCodeStr
showBinary int availableLen
  | nBins > availableLen =
    Left $
      "Couldn't fit binary " ++ binString bins ++ " (length: "
        ++ show nBins
        ++ ") into "
        ++ show availableLen
        ++ " bits of space"
  | int >= 0 = Right $ binString expandedBins
  | otherwise = Right $ binString $ twosComplement expandedBins
  where
    bins = binary (abs int)
    nBins = length bins
    expandedBins = replicate (availableLen - length bins) 0 ++ bins