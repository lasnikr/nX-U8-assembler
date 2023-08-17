{-# LANGUAGE DerivingStrategies #-}
module Main (main) where

import System.Console.Haskeline;
import System.Environment
import Data.Char
import Control.Monad.IO.Class

type MachineCode = String 

main :: IO ()
main = do
   args <- getArgs
   case args of
      (filePath:_)   -> getFile filePath
      _              -> interactiveMode

interactiveMode :: IO ()
interactiveMode =
   runInputT defaultSettings loop
   where
       loop :: InputT IO () 
       loop = do
            minput <- getInputLine "> "
            case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                           liftIO $ putStrLn $ parseLine input
                           loop

getFile :: String -> IO ()
getFile filePath = do
   contents <- readFile filePath
   putStrLn $ concatMap (parseLine . map toLower) (lines contents)

parseLine :: String -> MachineCode
parseLine line 
   | null line = ""
   | otherwise = concat $ splitByCommata line

splitByCommata :: String -> [String]
splitByCommata s = case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitByCommata s''
                            where (w, s'') = break (==',') s'