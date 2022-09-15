module Textus where

import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection, close, open)
import           Polysemy               (Members, Sem, runM)
import           Textus.DB              (Commentary, DB, Word, interpretDB,
                                         readAllBookCommentaries,
                                         readAllBookLatinVerses,
                                         readAllBookWords, toVolume, Latin)
import           Textus.Log             (Log, interpretLog, logDebug)
import           Textus.Mustache        (interpretMustache, renderTemplate)

getJohnLatinVerses :: Members '[DB, Log] r => Connection -> Sem r [Textus.DB.Latin]
getJohnLatinVerses conn = do
  johnVerses <- readAllBookLatinVerses conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnVerses) <> " verses."
  return johnVerses

getJohnWords :: Members '[DB, Log] r => Connection -> Sem r [Textus.DB.Word]
getJohnWords conn = do
  johnWords <- readAllBookWords conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnWords) <> " words."
  return johnWords

getJohnComments :: Members '[DB, Log] r => Connection -> Sem r [Textus.DB.Commentary]
getJohnComments conn = do
  johnComments <- readAllBookCommentaries conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnComments) <> " comments."
  return johnComments

app :: IO ()
app = do
  conn <- open "db.sqlite"
  runM . interpretDB . interpretLog . interpretMustache $ do
    ls <- getJohnLatinVerses conn
    ws <- getJohnWords conn
    cs <- getJohnComments conn
    renderTemplate $ toVolume ws cs ls
    -- logDebug . pack . show $ toVolume ws cs
  close conn
