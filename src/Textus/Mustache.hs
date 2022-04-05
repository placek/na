{-# LANGUAGE DeriveGeneric #-}

module Textus.Mustache where

import Polysemy ( embed, interpret, makeSem, Embed, Sem, Member )
import Data.Aeson ( ToJSON (toJSON) )
import Text.Mustache ( compileMustacheFile, renderMustache )
import qualified Data.Text.Lazy.IO as TIO

data Mustache m a where
  RenderTemplate :: ToJSON d => d -> String -> String -> Mustache m ()

makeSem ''Mustache

interpretMustache :: Member (Embed IO) r => Sem (Mustache ': r) a -> Sem r a
interpretMustache = interpret \case
  RenderTemplate model main side -> embed $ do
    mainTemplate <- compileMustacheFile main
    sideTemplate <- compileMustacheFile side
    let template = mainTemplate <> sideTemplate
    TIO.putStrLn $ renderMustache template (toJSON model)
