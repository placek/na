module Textus where

import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection, close, open)
import           Polysemy               (Members, Sem, runM)
import           Textus.DB              (Commentary, DB, Word, interpretDB,
                                         readAllBookCommentaries,
                                         readAllBookWords, toVolume)
import           Textus.Log             (Log, interpretLog, logDebug)
import           Textus.Mustache        (interpretMustache, renderTemplate)
import           Textus.VersesDB        (Verse, VersesDB, interpretVersesDB,
                                         readAllBookVerses)

getJohnVerses :: Members '[VersesDB, Log] r => Connection -> Sem r [Textus.VersesDB.Verse]
getJohnVerses conn = do
  johnVerses <- readAllBookVerses conn 500
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
  runM . interpretDB . interpretVersesDB . interpretLog . interpretMustache $ do
    -- vs <- getJohnVerses conn
    -- logDebug . pack .show $ vs
    ws <- getJohnWords conn
    cs <- getJohnComments conn
    renderTemplate $ toVolume ws cs
    -- logDebug . pack . show $ toVolume ws cs
  close conn
