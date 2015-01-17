module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.IO
import Data.List

import Language.BrainFuck.Compile
import Language.BrainFuck.Parse

main = do
  (inHdl, outHdl) <- getHandles
  cProgram <- liftM (compile . parse) (hGetContents inHdl)
  hPutStr outHdl cProgram


getHandles :: IO (Handle, Handle)
getHandles = do
  args <- filter (isSuffixOf ".b") <$> getArgs
  case args of
    []     -> return (stdin, stdout)
    (x:xs) -> do
      inHdl  <- openFile x ReadMode
      outHdl <- openFile (take (length x - 2) x ++ ".c") WriteMode
      return (inHdl, outHdl)
