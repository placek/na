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
  ReadAllBookVerses :: Connection -> BookNumber -> VersesDB m [Verse]
  ReadVerse         :: Connection -> BookNumber -> VerseNumber -> ChapterNumber -> VersesDB m (Maybe Verse)

makeSem ''VersesDB

interpretVersesDB :: Member (Embed IO) r => Sem (VersesDB ': r) a -> Sem r a
interpretVersesDB = interpret \case
  ReadAllBookVerses c bn -> embed $ queryNamed c "SELECT book_number, chapter, verse, text FROM verses WHERE book_number=:bn" [ ":bn" := bn ]
  ReadVerse c bn cn vn   -> embed $ listToMaybe <$> queryNamed c "SELECT * FROM verses WHERE book_number=:bn AND chapter=:cn AND verse=:vn" [ ":bn" := bn, ":cn" := cn, ":vn" := vn ]
