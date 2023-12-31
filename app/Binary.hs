module Binary
(   binString
,   binary
,   twosComplement
,   binStringToByteString
,   chunkToByte
,   chunksOf
,   littleEndian
,   isBit
,   binToDec
) where

import qualified Data.ByteString as B (pack, ByteString)
import Data.Foldable (foldl')
import Data.Char (digitToInt)

type MachineCodeStr = String

binStringToByteString :: MachineCodeStr -> B.ByteString
binStringToByteString code =
    B.pack $ map (fromIntegral . chunkToByte) $ littleEndian $ chunksOf 8 code

isBit :: Char -> Bool
isBit '1' = True
isBit '0' = True
isBit _ = False

chunkToByte :: MachineCodeStr -> Int
chunkToByte st = foldl' (\x acc -> x * 2 + acc ) 0 (map digitToInt st)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

littleEndian :: [MachineCodeStr] -> [MachineCodeStr]
littleEndian [] = []
littleEndian [x] = [x]
littleEndian (x:y:xs) = y : x : littleEndian xs

binString :: [Int] -> MachineCodeStr
binString = concatMap show

binary :: Int -> [Int]
binary 0 = []
binary x = binary (div x 2) ++ [mod x 2]

twosComplement :: [Int] -> [Int]
twosComplement bits = addOne (invert bits)

invert :: [Int] -> [Int]
invert = map (\bit -> if bit == 0 then 1 else 0)

addOne :: [Int] -> [Int]
addOne bits = reverse (addWithCarry (reverse bits) 1)

binToDec :: String -> Int
binToDec [] = 0
binToDec (x:xs) = digitToInt x * (2 ^ length xs) + binToDec xs

addWithCarry :: [Int] -> Int -> [Int]
addWithCarry [] carry = [1 | carry == 1]
addWithCarry (bit:bits) carry = sumBit : addWithCarry bits newCarry
    where
        sumBit = (bit + carry) `mod` 2
        newCarry = (bit + carry) `div` 2