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
          "add" -> [LookUp "r%n,r%m" "1000nnnnmmmm0001", LookUp "r%n,#i" "0001nnnniiiiiiii", LookUp "er%n,er%m" "1111nnn0>nmmm0>m0110", LookUp "er%n,#i" "1110nnn0>n1iiiiiii",
                    -- ADD as Control Register Access Instruction
                    LookUp "sp,#i" "11100001iiiiiiii"]
          "addc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0110", LookUp "r%n,#i" "0110nnnniiiiiiii"]
          "and" -> [LookUp "r%n,r%m" "1000nnnnmmmm0010", LookUp "r%n,#i" "0010nnnniiiiiiii"]
          "cmp" -> [LookUp "r%n,r%m" "1000nnnnmmmm0111", LookUp "r%n,#i" "0111nnnniiiiiiii", LookUp "er%n,er%m" "1111nnn0>nmmm00111"]
          "cpmc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0101", LookUp "r%n,#i" "0101nnnniiiiiiii"]
          "mov" -> [LookUp "r%n,r%m" "1000nnnnmmmm0000", LookUp "r%n,#i" "0000nnnniiiiiiii", LookUp "er%n, er%m" "1111mnnn0>nmm00101", LookUp "er%n,#i" "1110nnn0>n0iiiiiii",
                    -- MOV as Control Register Access Instruction
                    LookUp "ecsr,r%m" "10100000mmmm1111", LookUp "elr,er%m" "1010mmm0>m00001101", LookUp "epsw,r%m" "10100000mmmm1100", LookUp "er%n,elr" "1010nnn0>n00000101", LookUp "er%n,sp" "1010nnn0>n00011010", LookUp "psw,r%m" "10100000mmmm1011", LookUp "psw,#i" "11101001iiiiiiii", LookUp "r%n,ecsr" "1010nnnn00000111", LookUp "r%n,epsw" "1010nnnn00000100", LookUp "r%n,psw" "1010nnnn00000011", LookUp "sp,er%m" "10100001mmm0>m1010",
                    -- MOV as Coprocessor Data Transfer Instruction
                    LookUp "cr%n,r%m" "1010nnnnmmmm1110", LookUp "cer%n,:[ea]" "1111nnn0>n00101101", LookUp "cer%n,:[ea+]" "1111nnn0>n00111101", LookUp "cr%n,:[ea]" "1111nnnn00001101", LookUp "cr%n,:[ea+]" "1111nnnn00011101", LookUp "cxr%n,:[ea]" "1111nn0>n0>n01001101", LookUp "cxr%n,:[ea+]" "1111nn0>n0>n01011101", LookUp "cqr%n,:[ea]" "1111n0>n0>n0>n01101101", LookUp "cqr%n,:[ea+]" "1111n0>n0>n0>n01111101", 
                    LookUp "r%n,cr%m" "1010nnnnmmmm0110", LookUp ":[ea],cer%m" "1111mmm0>m10101101", LookUp ":[ea+],cer%m" "1111mmm0>m10111101", LookUp ":[ea],cr%m" "1111mmmm10001101", LookUp ":[ea+],cr%m" "1111mmmm10011101", LookUp ":[ea],cxr%m" "1111mm0>m0>m11001101", LookUp ":[ea+],cxr%m" "1111mm0>m0>m11011101", LookUp ":[ea],cqr%m" "1111m0>m0>m0>m11101101", LookUp ":[ea+],cqr%m" "1111m0>m0>m0>m11111101"]
          "or" -> [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,#i" "0011nnnniiiiiiii"]
          "xor" -> [LookUp "r%n,r%m" "1000nnnnmmmm0100", LookUp "r%n,%i" "0100nnnniiiiiiii"]
          "sub" -> [LookUp "r%n,r%m" "1000nnnnmmmm1000"]
          "subc" -> [LookUp "r%n,r%m" "1000nnnnmmmm1001"]
          -- Shift Instructions
          "sll" -> [LookUp "r%n,r%m" "1000nnnnmmmm1010", LookUp "r%n,%w" "1001nnnn0www1010"]
          "sllc" -> [LookUp "r%n,r%m" "1000nnnnmmmm1011", LookUp "r%n,%w" "1001nnnn0www1011"]
          "sra" -> [LookUp "r%n,r%m" "1000nnnnmmmm1110", LookUp "r%n,%w" "1001nnnn0www1110"]
          "srl" -> [LookUp "r%n,r%m" "1000nnnnmmmm1100", LookUp "r%n,%w" "1001nnnn0www1100"]
          "srlc" -> [LookUp "r%n,r%m" "1000nnnnmmmm1101", LookUp "r%n,%w" "1001nnnn0www1101"]
          -- Load Instructions
          "l" -> [LookUp "er%n,:[ea]" "1001nnn0>n00110010", LookUp "er%n,:[ea+]" "1001nnn0>n01010010", LookUp "er%n,:[er%m]" "1001nnn0>nmmm0>m0010", LookUp "er%n,:#d[er%m]" "1010nnn0>nmmm0>m1000dddddddddddddddd", LookUp "er%n,:#d[bp]" "1011nnn0>n00dddddd", LookUp "er%n,:#d[fp]" "1011nnn0>n01dddddd", LookUp "er%n,:#d" "1001nnn0>n00010010dddddddddddddddd",
                  LookUp "r%n,:[ea]" "1001nnnn00110000", LookUp "r%n,:[ea+]" "1001nnnn01010000", LookUp "r%n,:[er%m]" "1001nnnnmmm0>m0000", LookUp "r%n,:#d[er%m]" "1001nnnnmmm0>m1000dddddddddddddddd", LookUp "r%n,:#d[bp]" "1011nnnn00dddddd", LookUp "r%n,:#d[fp]" "1101nnnn01dddddd", LookUp "r%n,:#d" "1001nnnn00010000dddddddddddddddd",
                  LookUp "xr%n,:[ea]" "1001nn0>n0>n00110100", LookUp "xr%n,:[ea+]" "1001nn0>n0>n01010100", LookUp "qr%n,:[ea]" "1001n0>n0>n0>n00110110", LookUp "qr%n,:[ea+]" "1001n0>n0>n0>n01010110"]
          -- Store Instructions
          "st" -> [LookUp "er%n,:[ea]" "1001nnn0>n00110011", LookUp "er%n,:[ea+]" "1001nnn0>n01010011", LookUp "er%n,:[er%m]" "1001nnn0>nmmm0>m0011", LookUp "er%n,:#d[er%m]" "1010nnn0>nmmm0>m1001dddddddddddddddd", LookUp "er%n,:#d[bp]" "1011nnn0>n10dddddd", LookUp "er%n,:#d[fp]" "1011nnn0>n11dddddd", LookUp "er%n,:#d" "1001nnn0>n00010011dddddddddddddddd",
                  LookUp "r%n,:[ea]" "1001nnnn00110001", LookUp "r%n,:[ea+]" "1001nnnn01010001", LookUp "r%n,:[er%m]" "1001nnnnmmm0>m0001", LookUp "r%n,:#d[er%m]" "1001nnnnmmm0>m1001dddddddddddddddd", LookUp "r%n,:#d[bp]" "1011nnnn10dddddd", LookUp "r%n,:#d[fp]" "1101nnnn11dddddd", LookUp "r%n,:#d" "1001nnnn00010001dddddddddddddddd",
                  LookUp "xr%n,:[ea]" "1001nn0>n0>n00110101", LookUp "xr%n,:[ea+]" "1001nn0>n0>n01010101", LookUp "qr%n,:[ea]" "1001n0>n0>n0>n00110111", LookUp "qr%n,:[ea+]" "1001n0>n0>n0>n01010111"]
          -- PUSH/POP Instructions (TODO: registerlist)
          "push" -> [LookUp "er%n" "1111nnn0>n01011110", LookUp "qr%n" "1111n0>n0>n0>n01111110", LookUp "r%n" "1111nnnn01001110", LookUp "xr%n" "1111nn0>n0>n01101110"]
          "pop" -> [LookUp "er%n" "1111nnn0>n00011110", LookUp "qr%n" "1111n0>n0>n0>n00111110", LookUp "r%n" "1111nnnn00001110", LookUp "xr%n" "1111nn0>n0>n01001110" ]
          -- EA Register Data Transfer Instructions
          "lea" -> [LookUp ":[er%m]" "11110000mmm0>m1010", LookUp ":#d[er%m]" "11110000mmm01011dddddddddddddddd", LookUp ":#d" "1111000000001100dddddddddddddddd"]
          -- ALU Instructions
          "daa" -> [LookUp "r%n" "1000nnnn00011111"]
          "das" -> [LookUp "r%n" "1000nnnn00111111"]
          "neg" -> [LookUp "r%n" "1000nnnn01011111"]
          -- Bit Access Instructions
          "sb" -> [LookUp "r%n.%b" "1010nnnn0bbb0000", LookUp ":#d.%b" "101000001bbb0000dddddddddddddddd"]
          "rb" -> [LookUp "r%n.%b" "1010nnnn0bbb0010", LookUp ":#d.%b" "101000001bbb0010dddddddddddddddd"]
          "tb" -> [LookUp "r%n.%b" "1010nnnn0bbb0001", LookUp ":#d.%b" "101000001bbb0001dddddddddddddddd"]
          "ei" -> [LookUp "" "1110110100001000"]
          "di" -> [LookUp "" "1110101111110111"]
          "sc" -> [LookUp "" "1110110110000000"]
          "rc" -> [LookUp "" "1110101101111111"]
          "cplc" -> [LookUp "" "1111111011001111"]
          -- Conditional Relative Branch Instructions
          "bge" -> [LookUp "#r" "11000000rrrrrrrr"]
          "blt" -> [LookUp "#r" "11000001rrrrrrrr"]
          "bgt" -> [LookUp "#r" "11000010rrrrrrrr"]
          "ble" -> [LookUp "#r" "11000011rrrrrrrr"]
          "bges" -> [LookUp "#r" "11000100rrrrrrrr"]
          "blts" -> [LookUp "#r" "11000101rrrrrrrr"]
          "bgts" -> [LookUp "#r" "11000110rrrrrrrr"]
          "bles" -> [LookUp "#r" "11000111rrrrrrrr"]
          "bne" -> [LookUp "#r" "11001000rrrrrrrr"]
          "beq" -> [LookUp "#r" "11001001rrrrrrrr"]
          "bnv" -> [LookUp "#r" "11001010rrrrrrrr"]
          "bov" -> [LookUp "#r" "11001011rrrrrrrr"]
          "bps" -> [LookUp "#r" "11001100rrrrrrrr"]
          "bns" -> [LookUp "#r" "11001101rrrrrrrr"]
          "bal" -> [LookUp "#r" "11001110rrrrrrrr"]
          -- Sign Extension Instruction (TODO: only three bits)
          "extbw" -> [LookUp "er%n" "1000nnn1nnn01111"]
          -- Software Interrupt Instructions
          "swi" -> [LookUp "%i" "1110010100iiiiii"]
          "brk" -> [LookUp "" "1111111111111111"]
          -- Branch Instructions (TODO: find out how Cadr works)
          "b" -> [LookUp "er%n" "11110000nnn0>n0010"]
          "bl" -> [LookUp "er%n" "11110000nnn0>n0011"]
          -- Multiplication and Division Instructions
          "mul" -> [LookUp "er%n,r%m" "1111nnn0>nmmmm0100"]
          "div" -> [LookUp "er%n,r%m" "1111nnn0>nmmmm1001"]
          -- Miscellaneous
          "inc" -> [LookUp ":[ea]" "1111111000101111"]
          "dec" -> [LookUp ":[ea]" "1111111000111111"]
          "rt" -> [LookUp "" "1111111000011111"]
          "rti" -> [LookUp "" "1111111000001111"]
          "nop" -> [LookUp "" "1111111010001111"]
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
        ('%' : restLine) -> consumeVariable holder restLine isBit binToDec
        _ -> case consumeVariable holder line isHexDigit readHex' of
          Just res -> case res of
            (vs, 'h':lineLeft) -> Just (vs, lineLeft)
            _ -> Nothing
          Nothing -> consumeVariable holder line isDigit read
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

consumeDSR :: String -> Maybe (String, DSR)
consumeDSR line =
  case line of
    ('d' : 's' : 'r' : ':' : r) ->
      Just (r, "1111111010011111")
    ('r' : r) ->
      case span isDigit r of
        ([], _) -> Nothing
        (nums, ':' : rest) -> Just (rest, bin (read nums) 4 "10010000dddd1111")
        _ -> Nothing
    _ ->
      case span isHexDigit line of
        ([], _) -> case span isDigit line of
          ([], _) -> Nothing
          (nums, ':' : rest) -> Just (rest, bin (read nums) 8 "11100011dddddddd")
          _ -> Nothing
        (nums, 'h' : ':' : rest) -> Just (rest, bin (readHex' nums) 8 "11100011dddddddd")
        _ -> case span isDigit line of
          ([], _) -> Nothing
          (nums, ':' : rest) -> Just (rest, bin (read nums) 8 "11100011dddddddd")
          _ -> Nothing
  where 
    bin digits width patt =
      case showBinary digits width of
        Right i -> fromMaybe "" (replaceChars patt 'd' i)
        _ -> ""

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