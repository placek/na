{-# LANGUAGE DeriveGeneric #-}

module Textus.WordsDB where

import Data.Aeson
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

instance ToJSON Textus.WordsDB.Word

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data WordsDB m a where
  ReadAllBookWords :: Connection -> BookNumber -> WordsDB m [Word]
  ReadWord         :: Connection -> BookNumber -> ChapterNumber -> VerseNumber -> PositionNumber -> WordsDB m (Maybe Word)

makeSem ''WordsDB

interpretWordsDB :: Member (Embed IO) r => Sem (WordsDB ': r) a -> Sem r a
interpretWordsDB = interpret \case
  ReadAllBookWords c bn  -> embed $ queryNamed c "SELECT * FROM words WHERE book=:bn" [ ":bn" := bn ]
  ReadWord c bn cn vn pn -> embed $ listToMaybe <$> queryNamed c "SELECT * FROM words WHERE book=:bn AND chapter=:cn AND verse=:vn AND position=:pn" [ ":bn" := bn, ":cn" := cn, ":vn" := vn, ":pn" := pn ]
