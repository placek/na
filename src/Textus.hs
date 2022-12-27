module Textus where

import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection, close, open)
import           Polysemy               (Members, Sem, runM)
import           Textus.DB              (BookID (..))
import           Textus.Log             (Log, interpretLog, logDebug, logInfo)
import           Textus.Mustache        (interpretMustache, renderTemplate)
import           Textus.Typeset.DB      as TDB (Commentary, DB, Latin,
                                                Reference, Word, interpretDB,
                                                readAllBookCommentaries,
                                                readAllBookLatinVerses,
                                                readAllBookReferences,
                                                readAllBookWords, toVolume)
import qualified Textus.Compare.DB as CDB

getVerses :: Members '[CDB.DB, Log] r => Connection -> BookID -> Sem r [CDB.Verse]
getVerses conn book = do
  entities <- CDB.readAllBookVerses conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " verses."
  return entities

getLatinVerses :: Members '[TDB.DB, Log] r => Connection -> BookID -> Sem r [TDB.Latin]
getLatinVerses conn book = do
  entities <- TDB.readAllBookLatinVerses conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " verses."
  return entities

getWords :: Members '[TDB.DB, Log] r => Connection -> BookID -> Sem r [TDB.Word]
getWords conn book = do
  entities <- TDB.readAllBookWords conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " words."
  return entities

getComments :: Members '[TDB.DB, Log] r => Connection -> BookID -> Sem r [TDB.Commentary]
getComments conn book = do
  entities <- TDB.readAllBookCommentaries conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " comments."
  return entities

getReferences :: Members '[TDB.DB, Log] r => Connection -> BookID -> Sem r [TDB.Reference]
getReferences conn book = do
  entities <- TDB.readAllBookReferences conn book
  logDebug $ "found " <> (pack . show . Prelude.length $ entities) <> " references."
  return entities

typeset :: String -> IO ()
typeset bookID = do
  conn <- open "db.sqlite"
  runM . TDB.interpretDB . interpretLog . interpretMustache $ do
    ls <- getLatinVerses conn $ read bookID
    ws <- getWords conn $ read bookID
    cs <- getComments conn $ read bookID
    rs <- getReferences conn $ read bookID
    renderTemplate $ TDB.toVolume ws rs cs ls
  close conn

compare :: String -> IO ()
compare bookID = do
  db1 <- open "db/01.sqlite"
  db2 <- open "db/02.sqlite"
  db3 <- open "db/03.sqlite"
  runM . CDB.interpretDB . interpretLog $ do
    vs1 <- getVerses db1 $ read bookID
    vs2 <- getVerses db2 $ read bookID
    vs3 <- getVerses db3 $ read bookID
    logInfo . pack . show . CDB.fromVerses $ vs2 ++ vs1 ++ vs3 -- intended order
    return ()
  close db1
  close db2
  close db3
