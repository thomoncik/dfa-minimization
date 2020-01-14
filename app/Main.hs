module Main where

import System.Environment
import System.IO

import AutomatonUtils
import NFA

main :: IO ()
main = do
  argCount <- length <$> getArgs
  if argCount /= 2
    then putStrLn
           "Usage:\n\tdfa-minimization SOURCE DEST\nMinimize automaton from SOURCE and write result in DEST"
    else do
      (inputFilename:outputFilename:_) <- getArgs
      input <- readFile inputFilename
      writeFile
        outputFilename
        (show $ minimize . toDfa $ (read input :: NFA Char Int))
