{-# LANGUAGE DeriveGeneric #-}

module Textus.Mustache where

import Polysemy ( embed, interpret, makeSem, Embed, Sem, Member )
import Data.Aeson ( ToJSON (toJSON) )
import Text.Mustache ( compileMustacheFile, renderMustache )
import qualified Data.Text.Lazy.IO as TIO

data Mustache m a where
  RenderTemplate :: ToJSON d => d -> Mustache m ()

makeSem ''Mustache

interpretMustache :: Member (Embed IO) r => Sem (Mustache ': r) a -> Sem r a
interpretMustache = interpret \case
  RenderTemplate model -> embed $ do
    volumeTemplate  <- compileMustacheFile "templates/volume.mustache"
    bookTemplate    <- compileMustacheFile "templates/book.mustache"
    chapterTemplate <- compileMustacheFile "templates/chapter.mustache"
    verseTemplate   <- compileMustacheFile "templates/verse.mustache"
    wordTemplate    <- compileMustacheFile "templates/word.mustache"
    let template = volumeTemplate <> bookTemplate <> chapterTemplate <> verseTemplate <> wordTemplate
    TIO.putStrLn $ renderMustache template (toJSON model)
