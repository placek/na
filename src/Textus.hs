module Textus where

import Data.Text ( pack )
import Database.SQLite.Simple ( Connection, close, open )
import Polysemy ( runM, Members, Sem )
import Textus.Log ( Log, logDebug, interpretLog )
import Textus.WordsDB ( WordsDB, Word, readAllBookWords, interpretWordsDB, toVolume, Commentary, readAllBookCommentaries )
import Textus.Mustache ( interpretMustache, renderTemplate )

getJohnWords :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Word]
getJohnWords conn = do
  johnWords <- readAllBookWords conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnWords) <> " words."
  return johnWords

getJohnComments :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Commentary]
getJohnComments conn = do
  johnComments <- readAllBookCommentaries conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnComments) <> " words."
  return johnComments

app :: IO ()
app = do
  conn <- open "db.sqlite"
  runM . interpretWordsDB . interpretLog . interpretMustache $ do
    ws <- getJohnWords conn
    cs <- getJohnComments conn
    -- logDebug . pack . show $ toVolume ws
    renderTemplate $ toVolume ws cs
  close conn
