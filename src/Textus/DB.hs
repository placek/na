{-# LANGUAGE DeriveGeneric #-}

module Textus.DB where

import           Data.Aeson
import           Data.List
import           Data.Text
import           Database.SQLite.Simple
import           GHC.Generics           (Generic)
import           Polysemy
import           Prelude                hiding (Word)

type BookNumber     = Int
type ChapterNumber  = Int
type VerseNumber    = Int
type PositionNumber = Int

-- database data types
data Commentary =
  Commentary { cBookNumber        :: BookNumber
             , cChapterNumberFrom :: ChapterNumber
             , cVerseNumberFrom   :: VerseNumber
             , cChapterNumberTo   :: ChapterNumber
             , cVerseNumberTo     :: VerseNumber
             , cIsPreceding       :: Bool
             , cMarker            :: Text
             , cText              :: Text
             } deriving (Eq, Show, Generic)

data Latin =
  Latin { lBookNumber    :: BookNumber
        , lChapterNumber :: ChapterNumber
        , lVerseNumber   :: VerseNumber
        , lText          :: Text
        } deriving (Eq, Show, Generic)

data Word =
  Word { wId             :: Int
       , wBookNumber     :: BookNumber
       , wChapterNumber  :: ChapterNumber
       , wVerseNumber    :: VerseNumber
       , wPositionNumber :: PositionNumber
       , wText           :: Text
       , wStrong         :: Text
       , wMorphology     :: Text
       , wTranslation    :: Text
       , wFootnote       :: Text
       , wRed            :: Bool
       } deriving (Eq, Show, Generic)

-- structure data types
data Verse =
  Verse { verseNumber :: VerseNumber
        , words       :: [Textus.DB.Word]
        , comments    :: [Textus.DB.Commentary]
        , latin       :: Text
        } deriving (Eq, Show, Generic)

data Chapter =
  Chapter { chapterNumber :: ChapterNumber
          , verses        :: [Textus.DB.Verse]
          } deriving (Eq, Show, Generic)

data Book =
  Book { bookNumber :: BookNumber
       , chapters   :: [Textus.DB.Chapter]
       } deriving (Eq, Show, Generic)

newtype Volume = Volume { books :: [Textus.DB.Book] } deriving (Eq, Show, Generic)

-- ToJSON instances
instance ToJSON Textus.DB.Word
instance ToJSON Textus.DB.Verse
instance ToJSON Textus.DB.Chapter
instance ToJSON Textus.DB.Book
instance ToJSON Textus.DB.Volume
instance ToJSON Textus.DB.Commentary

-- FromRow instances
instance FromRow Commentary where
  fromRow = Commentary <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Latin where
  fromRow = Latin <$> field <*> field <*> field <*> field

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Ord instances
instance Ord Commentary where
  compare (Commentary a b c _ _ _ _ _) (Commentary d e f _ _ _ _ _) = compare (a, b, c) (d, e, f)

instance Ord Latin where
  compare (Latin a b c _) (Latin d e f _) = compare (a, b, c) (d, e, f)

instance Ord Word where
  compare (Word _ ba ca va pa _ _ _ _ _ _) (Word _ bb cb vb pb _ _ _ _ _ _) = compare (ba, ca, va, pa) (bb, cb, vb, pb)

instance Ord Verse where
  compare (Verse a _ _ _) (Verse b _ _ _) = compare a b

instance Ord Chapter where
  compare (Chapter a _) (Chapter b _) = compare a b

instance Ord Book where
  compare (Book a _) (Book b _) = compare a b

-- DB effect
data DB m a where
  ReadAllBookCommentaries :: Connection -> BookNumber -> DB m [Commentary]
  ReadAllBookLatinVerses  :: Connection -> BookNumber -> DB m [Latin]
  ReadAllBookWords        :: Connection -> BookNumber -> DB m [Word]

makeSem ''DB

interpretDB :: Member (Embed IO) r => Sem (DB ': r) a -> Sem r a
interpretDB = interpret \case
  ReadAllBookCommentaries c bn  -> embed $ queryNamed c "SELECT * FROM commentaries WHERE book_number=:bn" [ ":bn" := bn ]
  ReadAllBookLatinVerses  c bn  -> embed $ queryNamed c "SELECT book_number, chapter, verse, text FROM verses WHERE book_number=:bn" [ ":bn" := bn ]
  ReadAllBookWords        c bn  -> embed $ queryNamed c "SELECT * FROM words WHERE book=:bn" [ ":bn" := bn ]

-- structure constructor
toVolume :: [Word] -> [Commentary] -> Volume
toVolume ws cs = Volume $ toBooks ws cs

toBooks :: [Word] -> [Commentary] -> [Book]
toBooks ws cs = sort $ fmap mkBook wordsGroupedByBook
  where mkBook a                             = Book (bn a) (sort $ toChapters a (Prelude.filter (equal $ bn a) cs))
        bn a                                 = wBookNumber . Prelude.head $ a
        wordsGroupedByBook                   = Data.List.groupBy equalBookNumbers ws
        equalBookNumbers a b                 = wBookNumber a == wBookNumber b
        equal a (Commentary b _ _ _ _ _ _ _) = a == b

toChapters :: [Word] -> [Commentary] -> [Chapter]
toChapters ws cs = fmap mkChapter wordsGroupedByChapter
  where mkChapter a                          = Chapter (cn a) (sort $ toVerses a (Prelude.filter (equal $ cn a) cs))
        cn a                                 = wChapterNumber . Prelude.head $ a
        wordsGroupedByChapter                = Data.List.groupBy equalChapterNumbers ws
        equalChapterNumbers a b              = wChapterNumber a == wChapterNumber b
        equal a (Commentary _ c _ _ _ _ _ _) = a == c

toVerses :: [Word] -> [Commentary] -> [Verse]
toVerses ws cs = fmap mkVerse wordsGroupedByVerse
  where mkVerse a                            = Verse (vn a) (sort a) (Prelude.filter (equal $ vn a) cs) ""
        vn a                                 = wVerseNumber . Prelude.head $ a
        wordsGroupedByVerse                  = Data.List.groupBy equalVerseNumbers ws
        equalVerseNumbers a b                = wVerseNumber a == wVerseNumber b
        equal a (Commentary _ _ v _ _ _ _ _) = a == v
