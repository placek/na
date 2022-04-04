module Textus where

import Polysemy
import Database.SQLite.Simple

import Textus.Log
import Textus.VersesDB

getFirstJohn :: Members '[VersesDB, Log] r => Connection -> Sem r ()
getFirstJohn conn = do
  ver <- readVerse conn 500 1 1
  case ver of
    Nothing -> logWarn "nothing happened"
    Just v  -> logInfo $ "Found: " <> text v

saveSth :: Members '[VersesDB, Log] r => Connection -> Sem r ()
saveSth conn = do
  let ver = Verse 777 7 7 "Such glory! Much grace!"
  writeVerse conn ver

app :: IO ()
app = do
  conn <- open "test.db"
  runM . interpretVersesDB . interpretLog $ do
    getFirstJohn conn
    saveSth conn
  close conn
