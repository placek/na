module Main where

import           System.Environment (getArgs)
import           Textus             (compare)

main :: IO ()
main = do
  args <- getArgs
  Textus.compare $ head args
