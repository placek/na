{-# LANGUAGE DeriveGeneric #-}

module Textus.Mustache where

import           Data.Aeson        (ToJSON (toJSON))
import qualified Data.Text.Lazy.IO as TIO
import           Polysemy          (Embed, Member, Sem, embed, interpret,
                                    makeSem)
import           Text.Mustache     (compileMustacheFile, renderMustache)

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
