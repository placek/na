{-# LANGUAGE DeriveGeneric #-}

module Textus.WordsDB where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Polysemy
import Prelude hiding (Word)

type BookNumber     = Int
type ChapterNumber  = Int
type VerseNumber    = Int
type PositionNumber = Int

data Word =
  Word { wordId      :: Int
       , book        :: BookNumber
       , chapter     :: ChapterNumber
       , verse       :: VerseNumber
       , position    :: PositionNumber
       , text        :: Text
       , strong      :: Text
       , morphology  :: Text
       , translation :: Text
       , footnote    :: Text
       , version     :: Text
       , red         :: Bool
       } deriving (Eq, Show, Generic)

data Verse = Verse { verseNumber :: VerseNumber
                   , words :: [Textus.WordsDB.Word]
                   } deriving (Eq, Show, Generic)

data Chapter = Chapter { chapterNumber :: ChapterNumber
                       , verses :: [Textus.WordsDB.Verse]
                       } deriving (Eq, Show, Generic)

data Book = Book { bookNumber :: BookNumber
                 , chapters :: [Textus.WordsDB.Chapter]
                 } deriving (Eq, Show, Generic)

newtype Volume = Volume { books :: [Textus.WordsDB.Book] } deriving (Eq, Show, Generic)

instance ToJSON Textus.WordsDB.Word
instance ToJSON Textus.WordsDB.Verse
instance ToJSON Textus.WordsDB.Chapter
instance ToJSON Textus.WordsDB.Book
instance ToJSON Textus.WordsDB.Volume

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Ord Book where
  compare (Book a _) (Book b _) = compare a b

instance Ord Chapter where
  compare (Chapter a _) (Chapter b _) = compare a b

instance Ord Verse where
  compare (Verse a _) (Verse b _) = compare a b

instance Ord Word where
  compare (Word _ ba ca va pa _ _ _ _ _ _ _) (Word _ bb cb vb pb _ _ _ _ _ _ _) = compare (ba, ca, va, pa) (bb, cb, vb, pb)

data WordsDB m a where
  ReadAllBookWords :: Connection -> BookNumber -> WordsDB m [Word]
  ReadWord         :: Connection -> BookNumber -> ChapterNumber -> VerseNumber -> PositionNumber -> WordsDB m (Maybe Word)

toVolume :: [Word] -> Volume
toVolume values = Volume $ toBooks values

toBooks :: [Word] -> [Book]
toBooks values = sort $ fmap mkBook wordsGroupedByBook
  where mkBook ws = Book (getBookNumber ws) (sort $ toChapters ws)
        getBookNumber ws = book . Prelude.head $ ws
        wordsGroupedByBook = Data.List.groupBy equalBookNumbers values
        equalBookNumbers a b = book a == book b

toChapters :: [Word] -> [Chapter]
toChapters values = fmap mkChapter wordsGroupedByChapter
  where mkChapter ws = Chapter (getChapterNumber ws) (sort $ toVerses ws)
        getChapterNumber ws = chapter . Prelude.head $ ws
        wordsGroupedByChapter = Data.List.groupBy equalChapterNumbers values
        equalChapterNumbers a b = chapter a == chapter b

toVerses :: [Word] -> [Verse]
toVerses values = fmap mkVerse wordsGroupedByVerse
  where mkVerse ws = Verse (getVerseNumber ws) (sort ws)
        getVerseNumber ws = verse . Prelude.head $ ws
        wordsGroupedByVerse = Data.List.groupBy equalVerseNumbers values
        equalVerseNumbers a b = verse a == verse b

makeSem ''WordsDB

interpretWordsDB :: Member (Embed IO) r => Sem (WordsDB ': r) a -> Sem r a
interpretWordsDB = interpret \case
  ReadAllBookWords c bn  -> embed $ queryNamed c "SELECT * FROM words WHERE book=:bn" [ ":bn" := bn ]
  ReadWord c bn cn vn pn -> embed $ listToMaybe <$> queryNamed c "SELECT * FROM words WHERE book=:bn AND chapter=:cn AND verse=:vn AND position=:pn" [ ":bn" := bn, ":cn" := cn, ":vn" := vn, ":pn" := pn ]
