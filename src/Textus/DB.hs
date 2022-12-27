module Textus.DB where

data BookID = Mt | Mk | Lk | J deriving Read

type BookNumber     = Int
type ChapterNumber  = Int
type VerseNumber    = Int
type PositionNumber = Int

toNumber :: BookID -> BookNumber
toNumber Mt = 470
toNumber Mk = 480
toNumber Lk = 490
toNumber J  = 500
