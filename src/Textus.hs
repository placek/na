{-# LANGUAGE DeriveGeneric #-}

module Textus where

import Data.Aeson ( ToJSON )
import Data.Text ( pack )
import Database.SQLite.Simple ( Connection, close, open )
import GHC.Generics ( Generic )
import Polysemy ( runM, Members, Sem )
import Textus.Log ( Log, logDebug, interpretLog )
import Textus.WordsDB ( WordsDB, Word, readAllBookWords, interpretWordsDB )
import Textus.Mustache ( interpretMustache, renderTemplate )

newtype Book = Book { words :: [Textus.WordsDB.Word] } deriving (Eq, Show, Generic)

instance ToJSON Book

getJohnWords :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Word]
getJohnWords conn = do
  johnWords <- readAllBookWords conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnWords) <> " words."
  return johnWords

app :: IO ()
app = do
  conn <- open "db.sqlite"
  runM . interpretWordsDB . interpretLog . interpretMustache $ do
    ws <- getJohnWords conn
    let book = Book ws
    renderTemplate book "book.mustache" "word.mustache"
  close conn
