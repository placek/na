{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, OverloadedStrings, PolyKinds, ScopedTypeVariables #-}

module Textus where

import Polysemy
import Data.Maybe
import Data.Text
import Database.SQLite.Simple

type BookNumber    = Int
type ChapterNumber = Int
type VerseNumber   = Int

data Verse =
  Verse { book    :: BookNumber
        , chapter :: ChapterNumber
        , verse   :: VerseNumber
        , text    :: Text
        } deriving (Show)

instance FromRow Verse where
  fromRow = Verse <$> field <*> field <*> field <*> field

data VersesDB m a where
  ReadVerse  :: Connection -> BookNumber -> VerseNumber -> ChapterNumber -> VersesDB m (Maybe Verse)
  WriteVerse :: Connection -> Verse -> VersesDB m ()

makeSem ''VersesDB

interpretVersesDB :: Member (Embed IO) r => Sem (VersesDB ': r) a -> Sem r a
interpretVersesDB = interpret \case
  ReadVerse c bn cn vn            -> embed $ listToMaybe <$> queryNamed c "SELECT * FROM verses WHERE book_number=:bn AND chapter=:cn AND verse=:vn" [ ":bn" := bn, ":cn" := cn, ":vn" := vn ]
  WriteVerse c (Verse bn cn vn t) -> embed $ executeNamed c "INSERT INTO verse (book_number, chapter, verse) VALUES (:bn, :cn, :vn, :t)" [ ":bn" := bn, ":cn" := cn, ":vn" := vn, ":t" := t ]

data Log m a where
  LogError :: Text -> Log m ()
  LogDebug :: Text -> Log m ()

makeSem ''Log

interpretLog :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
interpretLog = interpret \case
  LogError msg -> embed . putStrLn . unpack $ "[ERROR] " <> msg
  LogDebug msg -> embed . putStrLn . unpack $ "[DEBUG] " <> msg

---- %< ----

getFirstJohn :: Members '[VersesDB, Log] r => Connection -> Sem r ()
getFirstJohn conn = do
  verse <- readVerse conn 500 1 1
  case verse of
    Nothing -> logError "nothing happened"
    Just v  -> logDebug $ "Found: " <> text v

app :: IO ()
app = do
  conn <- open "test.db"
  runM . interpretVersesDB . interpretLog $ getFirstJohn conn
  close conn
