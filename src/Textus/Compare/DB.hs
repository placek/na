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

-- database data types
data Verse =
  Verse
    { bookNumber    :: BookNumber
    , chapterNumber :: ChapterNumber
    , verseNumber   :: VerseNumber
    , text          :: Text
    } deriving (Eq, Show, Generic)

-- ToJSON instances
instance ToJSON Textus.Compare.DB.Verse

-- FromRow instances
instance FromRow Verse where
  fromRow = Verse <$> field <*> field <*> field <*> field

-- Ord instances
instance Ord Verse where
  compare (Verse a b c _) (Verse d e f _) = compare (a, b, c) (d, e, f)

-- DB effect
data DB m a where
  ReadAllBookVerses :: Connection -> BookID -> DB m [Verse]

makeSem ''DB

interpretDB :: Member (Embed IO) r => Sem (DB ': r) a -> Sem r a
interpretDB = interpret \case
  ReadAllBookVerses  c bid -> embed $ queryNamed c "SELECT * FROM `verses` WHERE `book_number`=:bn" [ ":bn" := toNumber bid ]
