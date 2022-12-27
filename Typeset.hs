module Main where

import           System.Environment (getArgs)
import           Textus             (typeset)

main :: IO ()
main = do
  args <- getArgs
  typeset $ head args
