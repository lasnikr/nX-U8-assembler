module Main (main) where

import System.Console.Haskeline;
import System.Environment
import Data.Char
import Control.Monad.IO.Class

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
   putStrLn $ parseLine (concat (lines contents))

parseLine :: String -> String
parseLine = parseToken . map toLower

parseToken :: String -> String 
parseToken token = case token of
   "add" -> "adding\n"
   _     -> "nothing\n"
