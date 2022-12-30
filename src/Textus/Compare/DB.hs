module Textus.Compare.DB where

import           Data.Text                        (Text)
import           Database.SQLite.Simple           (Connection,
                                                   NamedParam ((:=)),
                                                   queryNamed)
import           Database.SQLite.Simple.FromField
import           Polysemy                         (Embed, Member, Sem, embed,
                                                   interpret, makeSem)
import           Text.Regex.Pcre2                 (gsub)
import           Textus.DB

data DB m a where
  ReadAllBookVerses :: (FromField b)
                    => Connection
                    -> BookID
                    -> DB m [Value b]

makeSem ''DB

interpretDB :: (Member (Embed IO) r)
            => Sem (DB ': r) a
            -> Sem r a
interpretDB = interpret \case
  ReadAllBookVerses  c bid -> embed $ queryNamed c "SELECT * FROM `verses` WHERE `book_number`=:bn" [ ":bn" := toNumber bid ]

-- sanitization of the text
type Filter = [(Text, Text)]

readersFilter :: Filter
readersFilter = [ ("<f>.*?</f>",   ""   )
                , ("<S>.*?</S>",   ""   )
                , ("<m>.*?</m>",   ""   )
                , ("<e>(.*?)</e>", "\1" )
                , ("<pb/>",        ""   )
                , ("<t>(.*?)</t>", "\1" )
                , ("<br/>",        ""   )
                , ("\\s+",         " "  )
                ]

naFilter :: Filter
naFilter = [ ("<f>(.*?)</f>", "\1" )
           , ("<e>(.*?)</e>", "\1" )
           , ("<pb/>",        ""   )
           , ("\\*",          ""   )
           , ("\\[(.*?)\\]",  "\1" )
           , ("<t>(.*?)</t>", "\1" )
           , ("\\s+",         " "  )
           ]

polishFilter :: Filter
polishFilter = [ ("\\[.*?\\]", "" )
               , ("\\s+",      " " )
               ]

sanitizeVolume :: Filter
             -> Volume Text
             -> Volume Text
sanitizeVolume = flip $ foldr applyFilter
  where applyFilter :: (Text, Text) -> Volume Text -> Volume Text
        applyFilter (pattern, replacement) b = fmap (gsub pattern replacement) b

-- Comparison
data Comparison =
  Comparison
    { naVolume            :: Volume Text
    , readersVolume       :: Volume Text
    , polishVolume        :: Volume Text
    , strongCodes         :: Volume Int
    , morphologyCodes     :: Volume Text
    , wordsOfJesusMarkers :: Volume Bool
    } deriving Show

data Word =
  Word
    { wAddress        :: Address
    , wNaText         :: Text
    , wReadersText    :: Text
    , wPolishText     :: Text
    , wStrongCode     :: Int
    , wMorphologyCode :: Text
    , wWordOfJesus    :: Bool
    } deriving Show

type Words = [Textus.Compare.DB.Word]

toWords :: Comparison -> Words
toWords (Comparison n r p s m j) = toWord <$> merge6 ns rs ps ss ms js
  where ns = toTuple <$> to n
        rs = toTuple <$> to r
        ps = toTuple <$> to p
        ss = toTuple <$> to s
        ms = toTuple <$> to m
        js = toTuple <$> to j
        toWord (a, (b, c, d, e, f, g)) = Word a b c d e f g
