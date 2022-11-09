{-# LANGUAGE DeriveGeneric #-}

module Textus.DB where

import           Data.Aeson             (ToJSON)
import           Data.List              (groupBy, sort)
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, FromRow (..),
                                         NamedParam ((:=)), field, queryNamed)
import           GHC.Generics           (Generic)
import           Polysemy               (Embed, Member, Sem, embed, interpret,
                                         makeSem)
import           Prelude                hiding (Word)
import Data.Text (intercalate)

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
        , lEusebeios     :: Int
        } deriving (Eq, Show, Generic)

data Word =
  Word { wBookNumber     :: BookNumber
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

data Reference =
  Reference { rFrom          :: Text
            , rTo            :: Text
            , rVote          :: Int
            , rBookNumber    :: BookNumber
            , rChapterNumber :: ChapterNumber
            , rVerseNumber   :: VerseNumber
            } deriving (Eq, Show, Generic)

-- structure data types
data Verse =
  Verse { verseNumber :: VerseNumber
        , words       :: [Textus.DB.Word]
        , comments    :: [Textus.DB.Commentary]
        , latin       :: Text
        , eusebeios   :: Int
        , references  :: Text
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
instance ToJSON Textus.DB.Reference

-- FromRow instances
instance FromRow Commentary where
  fromRow = Commentary <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Latin where
  fromRow = Latin <$> field <*> field <*> field <*> field <*> field

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Reference where
  fromRow = Reference <$> field <*> field <*> field <*> field <*> field <*> field

-- Ord instances
instance Ord Commentary where
  compare (Commentary a b c _ _ _ _ _) (Commentary d e f _ _ _ _ _) = compare (a, b, c) (d, e, f)

instance Ord Latin where
  compare (Latin a b c _ _) (Latin d e f _ _) = compare (a, b, c) (d, e, f)

instance Ord Word where
  compare (Word ba ca va pa _ _ _ _ _ _) (Word bb cb vb pb _ _ _ _ _ _) = compare (ba, ca, va, pa) (bb, cb, vb, pb)

instance Ord Reference where
  compare (Reference _ _ p a b c) (Reference _ _ q d e f) = compare (a, b, c, p) (d, e, f, q)

instance Ord Verse where
  compare (Verse a _ _ _ _ _) (Verse b _ _ _ _ _) = compare a b

instance Ord Chapter where
  compare (Chapter a _) (Chapter b _) = compare a b

instance Ord Book where
  compare (Book a _) (Book b _) = compare a b

-- DB effect
data DB m a where
  ReadAllBookCommentaries :: Connection -> BookNumber -> DB m [Commentary]
  ReadAllBookLatinVerses  :: Connection -> BookNumber -> DB m [Latin]
  ReadAllBookWords        :: Connection -> BookNumber -> DB m [Word]
  ReadAllBookReferences   :: Connection -> BookNumber -> DB m [Reference]

makeSem ''DB

interpretDB :: Member (Embed IO) r => Sem (DB ': r) a -> Sem r a
interpretDB = interpret \case
  ReadAllBookCommentaries c bn -> embed $ queryNamed c "SELECT * FROM `commentaries` WHERE `book`=:bn" [ ":bn" := bn ]
  ReadAllBookLatinVerses  c bn -> embed $ queryNamed c "SELECT * FROM `latin_verses` WHERE `book`=:bn" [ ":bn" := bn ]
  ReadAllBookWords        c bn -> embed $ queryNamed c "SELECT * FROM `words`        WHERE `book`=:bn" [ ":bn" := bn ]
  ReadAllBookReferences   c bn -> embed $ queryNamed c "SELECT * FROM `references`   WHERE `book`=:bn" [ ":bn" := bn ]

-- structure constructor
toVolume :: [Word] -> [Reference] -> [Commentary] -> [Latin] -> Volume
toVolume ws rs cs ls = Volume $ toBooks ws rs cs ls

toBooks :: [Word] -> [Reference] -> [Commentary] -> [Latin] -> [Book]
toBooks ws rs cs ls = sort $ fmap mkBook wordsGroupedByBook
  where mkBook a                              = Book (bn a) (sort $ toChapters a (rWithSameBookNumber a) (cWithSameBookNumber a) (lWithSameBookNumber a))
        cWithSameBookNumber a                 = Prelude.filter (cEqual $ bn a) cs
        lWithSameBookNumber a                 = Prelude.filter (lEqual $ bn a) ls
        rWithSameBookNumber a                 = Prelude.filter (rEqual $ bn a) rs
        bn a                                  = wBookNumber . Prelude.head $ a
        wordsGroupedByBook                    = Data.List.groupBy equalBookNumbers ws
        equalBookNumbers a b                  = wBookNumber a == wBookNumber b
        cEqual a (Commentary b _ _ _ _ _ _ _) = a == b
        lEqual a (Latin b _ _ _ _)            = a == b
        rEqual a (Reference _ _ _ b _ _)      = a == b

toChapters :: [Word] -> [Reference] -> [Commentary] -> [Latin] -> [Chapter]
toChapters ws rs cs ls = fmap mkChapter wordsGroupedByChapter
  where mkChapter a                           = Chapter (cn a) (sort $ toVerses a (rWithSameBookNumber a) (cWithSameBookNumber a) (lWithSameBookNumber a))
        cWithSameBookNumber a                 = Prelude.filter (cEqual $ cn a) cs
        lWithSameBookNumber a                 = Prelude.filter (lEqual $ cn a) ls
        rWithSameBookNumber a                 = Prelude.filter (rEqual $ cn a) rs
        cn a                                  = wChapterNumber . Prelude.head $ a
        wordsGroupedByChapter                 = Data.List.groupBy equalChapterNumbers ws
        equalChapterNumbers a b               = wChapterNumber a == wChapterNumber b
        cEqual a (Commentary _ c _ _ _ _ _ _) = a == c
        lEqual a (Latin _ c _ _ _)            = a == c
        rEqual a (Reference _ _ _ _ c _)      = a == c

toVerses :: [Word] -> [Reference] -> [Commentary] -> [Latin] -> [Verse]
toVerses ws rs cs ls = fmap mkVerse wordsGroupedByVerse
  where mkVerse a                             = Verse (vn a) (sort a) (cWithSameBookNumber a) (lVerseText a) (lVerseEusebeios a) (intercalate (", " :: Text) $ rTo <$> rWithSameBookNumber a)
        cWithSameBookNumber a                 = Prelude.filter (cEqual $ vn a) cs
        lWithSameBookNumber a                 = Prelude.filter (lEqual $ vn a) ls
        rWithSameBookNumber a                 = reverse $ sort $ Prelude.filter (rEqual $ vn a) rs
        lVerseText a                          = lText . Prelude.head . lWithSameBookNumber $ a
        lVerseEusebeios a                     = lEusebeios . Prelude.minimum . lWithSameBookNumber $ a
        vn a                                  = wVerseNumber . Prelude.head $ a
        wordsGroupedByVerse                   = Data.List.groupBy equalVerseNumbers ws
        equalVerseNumbers a b                 = wVerseNumber a == wVerseNumber b
        cEqual a (Commentary _ _ v _ _ _ _ _) = a == v
        lEqual a (Latin _ _ v _ _)            = a == v
        rEqual a (Reference _ _ _ _ _ v)      = a == v
