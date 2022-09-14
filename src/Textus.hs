module Textus where

import Data.Text ( pack )
import Database.SQLite.Simple ( Connection, close, open )
import Polysemy ( runM, Members, Sem )
import Textus.Log ( Log, logDebug, interpretLog )
import Textus.WordsDB ( WordsDB, Word, readAllBookWords, interpretWordsDB, Commentary, readAllBookCommentaries, toVolume )
import Textus.Mustache ( interpretMustache, renderTemplate )
import Textus.VersesDB (VersesDB, Verse, readAllBookVerses, interpretVersesDB)

getJohnVerses :: Members '[VersesDB, Log] r => Connection -> Sem r [Textus.VersesDB.Verse]
getJohnVerses conn = do
  johnVerses <- readAllBookVerses conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnVerses) <> " verses."
  return johnVerses

getJohnWords :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Word]
getJohnWords conn = do
  johnWords <- readAllBookWords conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnWords) <> " words."
  return johnWords

getJohnComments :: Members '[WordsDB, Log] r => Connection -> Sem r [Textus.WordsDB.Commentary]
getJohnComments conn = do
  johnComments <- readAllBookCommentaries conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnComments) <> " comments."
  return johnComments

app :: IO ()
app = do
  conn <- open "db.sqlite"
  runM . interpretWordsDB . interpretVersesDB . interpretLog . interpretMustache $ do
    -- vs <- getJohnVerses conn
    -- logDebug . pack .show $ vs
    ws <- getJohnWords conn
    cs <- getJohnComments conn
    renderTemplate $ toVolume ws cs
    -- logDebug . pack . show $ toVolume ws cs
  close conn
