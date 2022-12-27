module Textus where

import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection, close, open)
import           Polysemy               (Members, Sem, runM)
import           Textus.DB              (BookID (..), Commentary, DB, Latin,
                                         Reference, Word, interpretDB,
                                         readAllBookCommentaries,
                                         readAllBookLatinVerses,
                                         readAllBookReferences,
                                         readAllBookWords, toVolume)
import           Textus.Log             (Log, interpretLog, logDebug)
import           Textus.Mustache        (interpretMustache, renderTemplate)

getLatinVerses :: Members '[DB, Log] r => Connection -> BookID -> Sem r [Textus.DB.Latin]
getLatinVerses conn book = do
  entities <- readAllBookLatinVerses conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " verses."
  return entities

getWords :: Members '[DB, Log] r => Connection -> BookID -> Sem r [Textus.DB.Word]
getWords conn book = do
  entities <- readAllBookWords conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " words."
  return entities

getComments :: Members '[DB, Log] r => Connection -> BookID -> Sem r [Textus.DB.Commentary]
getComments conn book = do
  entities <- readAllBookCommentaries conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " comments."
  return entities

getReferences :: Members '[DB, Log] r => Connection -> BookID -> Sem r [Textus.DB.Reference]
getReferences conn book = do
  entities <- readAllBookReferences conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " references."
  return entities

typeset :: String -> IO ()
typeset bookID = do
  conn <- open "db.sqlite"
  runM . interpretDB . interpretLog . interpretMustache $ do
    ls <- getLatinVerses conn $ read bookID
    ws <- getWords conn $ read bookID
    cs <- getComments conn $ read bookID
    rs <- getReferences conn $ read bookID
    renderTemplate $ toVolume ws rs cs ls
  close conn
