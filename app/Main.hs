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
                    LookUp "sp,#i" "1110_0001_iiii_iiii"]
          "addc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0110", LookUp "r%n,#i" "0110nnnniiiiiiii"]
          "and" -> [LookUp "r%n,r%m" "1000nnnnmmmm0010", LookUp "r%n,#i" "0010nnnniiiiiiii"]
          "cmp" -> [LookUp "r%n,r%m" "1000nnnnmmmm0111", LookUp "r%n,#i" "0111nnnniiiiiiii", LookUp "er%n,er%m" "1111_nnn0>n_mmm0_0111"]
          "cpmc" -> [LookUp "r%n,r%m" "1000nnnnmmmm0101", LookUp "r%n,#i" "0101nnnniiiiiiii"]
          "mov" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_0000", LookUp "r%n,#i" "0000_nnnn_iiii_iiii", LookUp "er%n, er%m" "1111mnnn0>nmm00101", LookUp "er%n,#i" "1110nnn0>n0iiiiiii",
                    -- MOV as Control Register Access Instruction
                    LookUp "ecsr,r%m" "1010_0000_mmmm_1111", LookUp "elr,er%m" "1010_mmm0>m_0000_1101", LookUp "epsw,r%m" "1010_0000_mmmm_1100", LookUp "er%n,elr" "1010_nnn0>n_0000_0101", LookUp "er%n,sp" "1010_nnn0>n_0001_1010", LookUp "psw,r%m" "1010_0000_mmmm_1011", LookUp "psw,#i" "1110_1001_iiii_iiii", LookUp "r%n,ecsr" "1010_nnnn_0000_0111", LookUp "r%n,epsw" "1010_nnnn_0000_0100", LookUp "r%n,psw" "1010_nnnn_0000_0011", LookUp "sp,er%m" "1010_0001_mmm0>m_1010",
                    -- MOV as Coprocessor Data Transfer Instruction
                    LookUp "cr%n,r%m" "1010_nnnn_mmmm_1110", LookUp "cer%n,:[ea]" "1111_nnn0>n_0010_1101", LookUp "cer%n,:[ea+]" "1111_nnn0>n_0011_1101", LookUp "cr%n,:[ea]" "1111_nnnn_0000_1101", LookUp "cr%n,:[ea+]" "1111_nnnn_0001_1101", LookUp "cxr%n,:[ea]" "1111_nn0>n0>n_0100_1101", LookUp "cxr%n,:[ea+]" "1111_nn0>n0>n_0101_1101", LookUp "cqr%n,:[ea]" "1111_n0>n0>n0>n_0110_1101", LookUp "cqr%n,:[ea+]" "1111_n0>n0>n0>n_0111_1101", 
                    LookUp "r%n,cr%m" "1010_nnnn_mmmm_0110", LookUp ":[ea],cer%m" "1111_mmm0>m_1010_1101", LookUp ":[ea+],cer%m" "1111_mmm0>m_1011_1101", LookUp ":[ea],cr%m" "1111_mmmm_1000_1101", LookUp ":[ea+],cr%m" "1111_mmmm_1001_1101", LookUp ":[ea],cxr%m" "1111_mm0>m0>m_1100_1101", LookUp ":[ea+],cxr%m" "1111_mm0>m0>m_1101_1101", LookUp ":[ea],cqr%m" "1111_m0>m0>m0>m_1110_1101", LookUp ":[ea+],cqr%m" "1111_m0>m0>m0>m_1111_1101"]
          "or" -> [LookUp "r%n,r%m" "1000nnnnmmmm0011", LookUp "r%n,#i" "0011nnnniiiiiiii"]
          "xor" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_0100", LookUp "r%n,%i" "0100_nnnn_iiii_iiii"]
          "sub" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1000"]
          "subc" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1001"]
          -- Shift Instructions
          "sll" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1010", LookUp "r%n,%w" "1001_nnnn_0www_1010"]
          "sllc" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1011", LookUp "r%n,%w" "1001_nnnn_0www_1011"]
          "sra" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1110", LookUp "r%n,%w" "1001_nnnn_0www_1110"]
          "srl" -> [LookUp "r%n,r%m" "1000_nnnn_mmmm_1100", LookUp "r%n,%w" "1001_nnnn_0www_1100"]
          "srlc" -> [LookUp "r%n,r%m" "1000nnnnmmmm1101", LookUp "r%n,%w" "1001nnnn0www1101"]
          -- Load Instructions
          "l" -> [LookUp "er%n,:[ea]" "1001nnn0>n00110010", LookUp "er%n,:[ea+]" "1001nnn0>n01010010", LookUp "er%n,:[er%m]" "1001_nnn0>n_mmm0>m_0010", LookUp "er%n,:#d[er%m]" "1010_nnn0>n_mmm0>m_1000_dddd_dddd_dddd_dddd", LookUp "er%n,:#d[bp]" "1011_nnn0>n_00dd_dddd", LookUp "er%n,:#d[fp]" "1011_nnn0>n_01dd_dddd", LookUp "er%n,:#d" "1001_nnn0>n_0001_0010_dddd_dddd_dddd_dddd",
                  LookUp "r%n,:[ea]" "1001_nnnn_0011_0000", LookUp "r%n,:[ea+]" "1001_nnnn_0101_0000", LookUp "r%n,:[er%m]" "1001_nnnn_mmm0>m_0000", LookUp "r%n,:#d[er%m]" "1001_nnnn_mmm0>m_1000_dddd_dddd_dddd_dddd", LookUp "r%n,:#d[bp]" "1011_nnnn_00dd_dddd", LookUp "r%n,:#d[fp]" "1101_nnnn_01dd_dddd", LookUp "r%n,:#d" "1001_nnnn_0001_0000_dddd_dddd_dddd_dddd",
                  LookUp "xr%n,:[ea]" "1001_nn0>n0>n_0011_0100", LookUp "xr%n,:[ea+]" "1001_nn0>n0>n_0101_0100", LookUp "qr%n,:[ea]" "1001_n0>n0>n0>n_0011_0110", LookUp "qr%n,:[ea+]" "1001_n0>n0>n0>n_0101_0110"]
          -- Store Instructions
          "st" -> [LookUp "er%n,:[ea]" "1001nnn0>n00110011", LookUp "er%n,:[ea+]" "1001nnn0>n01010011", LookUp "er%n,:[er%m]" "1001_nnn0>n_mmm0>m_0011", LookUp "er%n,:#d[er%m]" "1010_nnn0>n_mmm0>m_1001_dddd_dddd_dddd_dddd", LookUp "er%n,:#d[bp]" "1011_nnn0>n_10dd_dddd", LookUp "er%n,:#d[fp]" "1011_nnn0>n_11dd_dddd", LookUp "er%n,:#d" "1001_nnn0>n_0001_0011_dddd_dddd_dddd_dddd",
                  LookUp "r%n,:[ea]" "1001_nnnn_0011_0001", LookUp "r%n,:[ea+]" "1001_nnnn_0101_0001", LookUp "r%n,:[er%m]" "1001_nnnn_mmm0>m_0001", LookUp "r%n,:#d[er%m]" "1001_nnnn_mmm0>m_1001_dddd_dddd_dddd_dddd", LookUp "r%n,:#d[bp]" "1011_nnnn_10dd_dddd", LookUp "r%n,:#d[fp]" "1101_nnnn_11dd_dddd", LookUp "r%n,:#d" "1001_nnnn_0001_0001_dddd_dddd_dddd_dddd",
                  LookUp "xr%n,:[ea]" "1001_nn0>n0>n_0011_0101", LookUp "xr%n,:[ea+]" "1001_nn0>n0>n_0101_0101", LookUp "qr%n,:[ea]" "1001_n0>n0>n0>n_0011_0111", LookUp "qr%n,:[ea+]" "1001_n0>n0>n0>n_0101_0111"]
          -- PUSH/POP Instructions (TODO: register_list)
          "push" -> [LookUp "er%n" "1111_nnn0>n_0101_1110", LookUp "qr%n" "1111_n0>n0>n0>n_0111_1110", LookUp "r%n" "1111_nnnn_0100_1110", LookUp "xr%n" "1111_nn0>n0>n_0110_1110"]
          "pop" -> [LookUp "er%n" "1111_nnn0>n_0001_1110", LookUp "qr%n" "1111_n0>n0>n0>n_0011_1110", LookUp "r%n" "1111_nnnn_0000_1110", LookUp "xr%n" "1111_nn0>n0>n_0100_1110" ]
          -- EA Register Data Transfer Instructions
          "lea" -> [LookUp ":[er%m]" "1111_0000_mmm0>m_1010", LookUp ":#d[er%m]" "1111_0000_mmm0_1011_dddd_dddd_dddd_dddd", LookUp ":#d" "1111_0000_0000_1100_dddd_dddd_dddd_dddd"]
          -- ALU Instructions
          "daa" -> [LookUp "r%n" "1000_nnnn_0001_1111"]
          "das" -> [LookUp "r%n" "1000_nnnn_0011_1111"]
          "neg" -> [LookUp "r%n" "1000_nnnn_0101_1111"]
          -- Bit Access Instructions
          "sb" -> [LookUp "r%n.%b" "1010_nnnn_0bbb_0000", LookUp ":#d.%b" "1010_0000_1bbb_0000_dddd_dddd_dddd_dddd"]
          "rb" -> [LookUp "r%n.%b" "1010_nnnn_0bbb_0010", LookUp ":#d.%b" "1010_0000_1bbb_0010_dddd_dddd_dddd_dddd"]
          "tb" -> [LookUp "r%n.%b" "1010_nnnn_0bbb_0001", LookUp ":#d.%b" "1010_0000_1bbb_0001_dddd_dddd_dddd_dddd"]
          "ei" -> [LookUp "" "1110_1101_0000_1000"]
          "di" -> [LookUp "" "1110_1011_1111_0111"]
          "sc" -> [LookUp "" "1110_1101_1000_0000"]
          "rc" -> [LookUp "" "1110_1011_0111_1111"]
          "cplc" -> [LookUp "" "1111_1110_1100_1111"]
          -- Conditional Relative Branch Instructions
          "bge" -> [LookUp ":#r" "1100_0000_rrrr_rrrr"]
          "blt" -> [LookUp ":#r" "1100_0001_rrrr_rrrr"]
          "bgt" -> [LookUp ":#r" "1100_0010_rrrr_rrrr"]
          "ble" -> [LookUp ":#r" "1100_0011_rrrr_rrrr"]
          "bges" -> [LookUp ":#r" "1100_0100_rrrr_rrrr"]
          "blts" -> [LookUp ":#r" "1100_0101_rrrr_rrrr"]
          "bgts" -> [LookUp ":#r" "1100_0110_rrrr_rrrr"]
          "bles" -> [LookUp ":#r" "1100_0111_rrrr_rrrr"]
          "bne" -> [LookUp ":#r" "1100_1000_rrrr_rrrr"]
          "beq" -> [LookUp ":#r" "1100_1001_rrrr_rrrr"]
          "bnv" -> [LookUp ":#r" "1100_1010_rrrr_rrrr"]
          "bov" -> [LookUp ":#r" "1100_1011_rrrr_rrrr"]
          "bps" -> [LookUp ":#r" "1100_1100_rrrr_rrrr"]
          "bns" -> [LookUp ":#r" "1100_1101_rrrr_rrrr"]
          "bal" -> [LookUp ":#r" "1100_1110_rrrr_rrrr"]
          -- Sign Extension Instruction (TODO: only three bits)
          "extbw" -> [LookUp "er%n" "1000_nnn1_nnn0_1111"]
          -- Software Interrupt Instructions
          "swi" -> [LookUp "%i" "1110_0101_00ii_iiii"]
          "brk" -> [LookUp "" "1111_1111_1111_1111"]
          -- Branch Instructions (TODO: find out how Cadr works)
          "b" -> [LookUp "er%n" "1111_0000_nnn0>n_0010"]
          "bl" -> [LookUp "er%n" "1111_0000_nnn0>n_0011"]
          -- Multiplication and Division Instructions
          "mul" -> [LookUp "er%n,r%m" "1111_nnn0>n_mmmm_0100"]
          "div" -> [LookUp "er%n,r%m" "1111_nnn0>n_mmmm_1001"]
          -- Miscellaneous
          "inc" -> [LookUp ":[ea]" "1111_1110_0010_1111"]
          "dec" -> [LookUp ":[ea]" "1111_1110_0011_1111"]
          "rt" -> [LookUp "" "1111_1110_0001_1111"]
          "rti" -> [LookUp "" "1111_1110_0000_1111"]
          "nop" -> [LookUp "" "1111_1110_1000_1111"]
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