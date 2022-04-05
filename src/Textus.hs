module Textus where

import Polysemy
import Database.SQLite.Simple
import Data.Text

import Textus.Log
-- import Textus.VersesDB
import Textus.WordsDB
import qualified Data.Text.IO

getJohnWords :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Word]
getJohnWords conn = do
  johnWords <- readAllBookWords conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnWords) <> " words."
  return johnWords

app :: IO ()
app = do
  conn <- open "db.sqlite"
  ws <- runM . interpretWordsDB . interpretLog $ getJohnWords conn
  Data.Text.IO.putStrLn . Data.Text.unwords . fmap text $ ws
  close conn
