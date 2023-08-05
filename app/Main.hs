{-# LANGUAGE DerivingStrategies #-}
module Main (main) where

import System.Console.Haskeline;
import System.Environment
import Data.Char
import Control.Monad.IO.Class

-- m_instruction, m_operand1, s_operand1, m_operand2, s_operand2
data Instruction = Instruction Integer Integer Integer Integer Integer | Error deriving stock (Show)

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
   putStrLn $ parseLine (map toLower $ concat (lines contents))

parseLine :: String -> String
parseLine x = show $ parseMnemonic $ head $ words x

parseMnemonic :: String -> Instruction
parseMnemonic x = case x of
   "sub" -> Instruction 0x8008 0x0000 0x0000 0x0000 0x0000
   _ -> Error
