{-# LANGUAGE DeriveGeneric #-}

module Textus.Compare.DB where

import           Data.Aeson             (ToJSON)
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, FromRow (..),
                                         NamedParam ((:=)), field, queryNamed)
import           GHC.Generics           (Generic)
import           Polysemy               (Embed, Member, Sem, embed, interpret,
                                         makeSem)
import           Prelude                hiding (Word)
import           Textus.DB              (BookID, BookNumber, ChapterNumber,
                                         VerseNumber, toNumber)
import Data.List (group, sort)

-- database data types
data Verse =
  Verse
    { vBookNumber    :: BookNumber
    , vChapterNumber :: ChapterNumber
    , vVerseNumber   :: VerseNumber
    , vText          :: Text
    } deriving (Show, Generic)

data Comparison =
  Comparison
    { cBookNumber    :: BookNumber
    , cChapterNumber :: ChapterNumber
    , cVerseNumber   :: VerseNumber
    , cNaText        :: Text
    , cReadersText   :: Text
    , cPolishText    :: Text
    } deriving (Eq, Show, Generic)

-- ToJSON instances
instance ToJSON Textus.Compare.DB.Comparison

-- FromRow instances
instance FromRow Verse where
  fromRow = Verse <$> field <*> field <*> field <*> field

-- instances
instance Eq Verse where
  (Verse a b c _) == (Verse d e f _) = a == d && b == e && c == f

instance Ord Verse where
  compare (Verse a b c _) (Verse d e f _) = compare (a, b, c) (d, e, f)

-- DB effect
data DB m a where
  ReadAllBookVerses :: Connection -> BookID -> DB m [Verse]

makeSem ''DB

interpretDB :: Member (Embed IO) r => Sem (DB ': r) a -> Sem r a
interpretDB = interpret \case
  ReadAllBookVerses  c bid -> embed $ queryNamed c "SELECT * FROM `verses` WHERE `book_number`=:bn" [ ":bn" := toNumber bid ]

-- utils

fromVerses :: [Verse] -> [Comparison]
fromVerses verses = toComparison <$> (group . sort) verses

toComparison :: [Verse] -> Comparison
toComparison [a, b, c] = Comparison (vBookNumber a) (vChapterNumber a) (vVerseNumber a) (vText a) (vText b) (vText c)
toComparison _         = error "nope"
