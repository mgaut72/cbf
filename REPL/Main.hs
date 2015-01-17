module Main where

import Control.Monad
import Language.BrainFuck.Parse
import Language.BrainFuck.Interpret

import System.IO

main = void $ repl emptyTape

repl :: Tape -> IO Tape
repl t = putStr "\n" >> readPrompt "bf>>> " >>= interpreter >>= repl
 where interpreter = flip (curry interpret') t . parse

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
