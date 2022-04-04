module Textus.VersesDB where

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
  WriteVerse c (Verse bn cn vn t) -> embed $ executeNamed c "INSERT INTO verses (book_number, chapter, verse, text) VALUES (:bn, :cn, :vn, :t)" [ ":bn" := bn, ":cn" := cn, ":vn" := vn, ":t" := t ]
