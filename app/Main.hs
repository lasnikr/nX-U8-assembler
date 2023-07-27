module Main (main) where

import System.Environment
import Data.Char

main :: IO ()
main = do
   args <- getArgs
   case args of
      (filePath:_)   -> getFile filePath
      _              -> putStrLn "[Interactive Mode]"

getFile :: String -> IO ()
getFile filePath = do
   contents <- readFile filePath
   putStrLn $ parseLine (lines contents)

parseLine :: [String] -> String
parseLine = concatMap (parseToken . map toLower)

parseToken :: String -> String
parseToken token = case token of
   "add" -> "adding\n"
   _     -> "nothing\n"