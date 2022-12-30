{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

module Textus.DB where

import           Data.Map                         (Map, fromList, toList)
import           Database.SQLite.Simple           (FromRow (fromRow), field)
import           Database.SQLite.Simple.FromField (FromField)

data BookID = Mt | Mk | Lk | J deriving (Show, Read, Eq, Ord)

data Value a = Value Address a deriving (Show)

type BookNumber     = Int
type ChapterNumber  = Int
type VerseNumber    = Int
type PositionNumber = Int
type Address        = (BookID, ChapterNumber, VerseNumber, PositionNumber)
type Volume a       = Map Address a

toNumber :: BookID -> BookNumber
toNumber Mt = 470
toNumber Mk = 480
toNumber Lk = 490
toNumber J  = 500

fromNumber :: BookNumber -> BookID
fromNumber 470 = Mt
fromNumber 480 = Mk
fromNumber 490 = Lk
fromNumber 500 = J
fromNumber _   = error "Unknown book ID"

type Values a = [Value a]

class Iso a b where
  to   :: a -> b
  from :: b -> a

toTuple :: Value a -> (Address, a)
toTuple (Value (b, c, v, p) t) = ((b, c, v, p), t)

toValue :: (Address, a) -> Value a
toValue ((b, c, v, p), t) = Value (b, c, v, p) t

instance (FromField a) => FromRow (Value a) where
  fromRow = do
    book    <- field
    chapter <- field
    verse   <- field
    Value (fromNumber book, chapter, verse, 0) <$> field

instance Iso (Volume a) (Values a) where
  to   volume = toValue <$> toList volume
  from values = fromList $ toTuple <$> values

merge :: Eq a
      => [(a, b)]
      -> [(a, c)]
      -> [(a, (b, c))]
merge s p = [ (w, (a, b)) | (w, a) <- s
                          , (x, b) <- p
                          , w == x
                          ]

merge3 :: Eq a
      => [(a, b)]
      -> [(a, c)]
      -> [(a, d)]
      -> [(a, (b, c, d))]
merge3 s p q = [ (w, (a, b, c)) | (w, a) <- s
                                , (x, b) <- p
                                , (y, c) <- q
                                , w == x
                                , x == y
                                ]

merge4 :: Eq a
      => [(a, b)]
      -> [(a, c)]
      -> [(a, d)]
      -> [(a, e)]
      -> [(a, (b, c, d, e))]
merge4 s p q r = [ (w, (a, b, c, d)) | (w, a) <- s
                                     , (x, b) <- p
                                     , (y, c) <- q
                                     , (z, d) <- r
                                     , w == x
                                     , x == y
                                     , y == z
                                     ]

merge5 :: Eq a
      => [(a, b)]
      -> [(a, c)]
      -> [(a, d)]
      -> [(a, e)]
      -> [(a, f)]
      -> [(a, (b, c, d, e, f))]
merge5 s p q r t = [ (w, (a, b, c, d, e)) | (w, a) <- s
                                          , (x, b) <- p
                                          , (y, c) <- q
                                          , (z, d) <- r
                                          , (u, e) <- t
                                          , u == w
                                          , w == x
                                          , x == y
                                          , y == z
                                          ]

merge6 :: Eq a
      => [(a, b)]
      -> [(a, c)]
      -> [(a, d)]
      -> [(a, e)]
      -> [(a, f)]
      -> [(a, g)]
      -> [(a, (b, c, d, e, f, g))]
merge6 s p q r t o = [ (w, (a, b, c, d, e, f)) | (w, a) <- s
                                               , (x, b) <- p
                                               , (y, c) <- q
                                               , (z, d) <- r
                                               , (u, e) <- t
                                               , (v, f) <- o
                                               , v == u
                                               , u == w
                                               , w == x
                                               , x == y
                                               , y == z
                                               ]
