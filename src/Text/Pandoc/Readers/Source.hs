module Text.Pandoc.Readers.Source
  (readSource)
  where

import Text.Pandoc.Definition
import Text.Pandoc.Shared 

readSource :: ParserState -> String -> Pandoc
readSource st text = Pandoc nullMeta [CodeBlock codeAttrs text]
  where
    nullMeta = Meta [] [] []
    codeAttrs = ("", classes, [])
    classes = stateIndentedCodeClasses st
