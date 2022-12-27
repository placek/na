module Main where

import           System.Environment (getArgs)
import           Textus             (typeset)

main :: IO ()
main = do
  args <- getArgs
  Textus.typeset $ head args
