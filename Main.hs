module Main where

import           System.Environment (getArgs)
import           Textus             (app)

main :: IO ()
main = do
  args <- getArgs
  app $ head args
