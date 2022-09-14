{-# LANGUAGE DeriveGeneric #-}

module Textus.WordsDB where

import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Data.Text
import           Database.SQLite.Simple
import           GHC.Generics           (Generic)
import           Polysemy
import           Prelude                hiding (Word)

type BookNumber     = Int
type ChapterNumber  = Int
type VerseNumber    = Int
type PositionNumber = Int

data Commentary =
  Commentary { cBookNumber        :: BookNumber
             , cChapterNumberFrom :: ChapterNumber
             , cVerseNumberFrom   :: VerseNumber
             , cChapterNumberTo   :: ChapterNumber
             , cVerseNumberTo     :: VerseNumber
             , isPreceding        :: Bool
             , marker             :: Text
             , cText              :: Text
             } deriving (Eq, Show, Generic)

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
       , red         :: Bool
       } deriving (Eq, Show, Generic)

data Verse =
  Verse { verseNumber :: VerseNumber
        , words       :: [Textus.WordsDB.Word]
        , comments    :: [Textus.WordsDB.Commentary]
        } deriving (Eq, Show, Generic)

data Chapter =
  Chapter { chapterNumber :: ChapterNumber
          , verses        :: [Textus.WordsDB.Verse]
          } deriving (Eq, Show, Generic)

data Book =
  Book { bookNumber :: BookNumber
       , chapters   :: [Textus.WordsDB.Chapter]
       } deriving (Eq, Show, Generic)

newtype Volume = Volume { books :: [Textus.WordsDB.Book] } deriving (Eq, Show, Generic)

instance ToJSON Textus.WordsDB.Word
instance ToJSON Textus.WordsDB.Verse
instance ToJSON Textus.WordsDB.Chapter
instance ToJSON Textus.WordsDB.Book
instance ToJSON Textus.WordsDB.Volume
instance ToJSON Textus.WordsDB.Commentary

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Commentary where
  fromRow = Commentary <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Ord Book where
  compare (Book a _) (Book b _) = compare a b

instance Ord Commentary where
  compare (Commentary a b c _ _ _ _ _) (Commentary d e f _ _ _ _ _) = compare (a, b, c) (d, e, f)

instance Ord Chapter where
  compare (Chapter a _) (Chapter b _) = compare a b

instance Ord Verse where
  compare (Verse a _ _) (Verse b _ _) = compare a b

instance Ord Word where
  compare (Word _ ba ca va pa _ _ _ _ _ _) (Word _ bb cb vb pb _ _ _ _ _ _) = compare (ba, ca, va, pa) (bb, cb, vb, pb)

data WordsDB m a where
  ReadAllBookWords        :: Connection -> BookNumber -> WordsDB m [Word]
  ReadAllBookCommentaries :: Connection -> BookNumber -> WordsDB m [Commentary]
  ReadWord                :: Connection -> BookNumber -> ChapterNumber -> VerseNumber -> PositionNumber -> WordsDB m (Maybe Word)

toVolume :: [Word] -> [Commentary] -> Volume
toVolume values cs = Volume $ toBooks values cs

toBooks :: [Word] -> [Commentary] -> [Book]
toBooks values cs = sort $ fmap mkBook wordsGroupedByBook
  where mkBook ws                               = Book (bn ws) (sort $ toChapters ws (Prelude.filter (equalBk ws) cs))
        bn ws                                   = book . Prelude.head $ ws
        wordsGroupedByBook                      = Data.List.groupBy equalBookNumbers values
        equalBookNumbers a b                    = book a == book b
        equalBk ws (Commentary b _ _ _ _ _ _ _) = b == bn ws

toChapters :: [Word] -> [Commentary] -> [Chapter]
toChapters values cs = fmap mkChapter wordsGroupedByChapter
  where mkChapter ws                            = Chapter (cn ws) (sort $ toVerses ws (Prelude.filter (equalCp ws) cs))
        cn ws                                   = chapter . Prelude.head $ ws
        wordsGroupedByChapter                   = Data.List.groupBy equalChapterNumbers values
        equalChapterNumbers a b                 = chapter a == chapter b
        equalCp ws (Commentary _ c _ _ _ _ _ _) = c == cn ws

toVerses :: [Word] -> [Commentary] -> [Verse]
toVerses values cs = fmap mkVerse wordsGroupedByVerse
  where mkVerse ws                              = Verse (vn ws) (sort ws) (Prelude.filter (equalVn ws) cs)
        vn ws                                   = verse . Prelude.head $ ws
        wordsGroupedByVerse                     = Data.List.groupBy equalVerseNumbers values
        equalVerseNumbers a b                   = verse a == verse b
        equalVn ws (Commentary _ _ v _ _ _ _ _) = v == vn ws

makeSem ''WordsDB

interpretWordsDB :: Member (Embed IO) r => Sem (WordsDB ': r) a -> Sem r a
interpretWordsDB = interpret \case
  ReadAllBookWords c bn  -> embed $ queryNamed c "SELECT * FROM words WHERE book=:bn" [ ":bn" := bn ]
  ReadAllBookCommentaries c bn  -> embed $ queryNamed c "SELECT * FROM commentaries WHERE book_number=:bn" [ ":bn" := bn ]
  ReadWord c bn cn vn pn -> embed $ listToMaybe <$> queryNamed c "SELECT * FROM words WHERE book=:bn AND chapter=:cn AND verse=:vn AND position=:pn" [ ":bn" := bn, ":cn" := cn, ":vn" := vn, ":pn" := pn ]
