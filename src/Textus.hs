module Textus where

import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection, close, open)
import           Polysemy               (Members, Sem, runM)
import           Textus.DB              (Commentary, DB, Word, interpretDB,
                                         readAllBookCommentaries,
                                         readAllBookLatinVerses,
                                         readAllBookWords, toVolume, Latin, readAllBookReferences, Reference)
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

getJohnReferences :: Members '[DB, Log] r => Connection -> Sem r [Textus.DB.Reference]
getJohnReferences conn = do
  johnComments <- readAllBookReferences conn 500
  logDebug $ "found " <> (pack . show . Prelude.length $ johnComments) <> " references."
  return johnComments

app :: IO ()
app = do
  conn <- open "db.sqlite"
  runM . interpretDB . interpretLog . interpretMustache $ do
    ls <- getJohnLatinVerses conn
    ws <- getJohnWords conn
    cs <- getJohnComments conn
    rs <- getJohnReferences conn
    renderTemplate $ toVolume ws rs cs ls
  close conn
