{-# LANGUAGE PatternGuards #-}
module Text.Pandoc.Writers.Asciidoc (writeAsciidoc) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Data.List (intercalate)
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

headerChars ::  M.Map Int Char
headerChars = M.fromList [(1,'='), (2,'-'), (3,'~'), (4,'^')]

writeAsciidoc :: WriterOptions -> Pandoc -> String
writeAsciidoc _ (Pandoc meta blocks) = 
  ascMeta meta ++ "\n\n" ++ ascBlocks blocks

header :: Int -> String -> String
header n title = "\n" ++ title ++ "\n" ++ sub ++ "\n"
  where
    sub = replicate l ch
    ch  = fromMaybe '=' $ M.lookup n headerChars
    l   = length title

para :: String -> String
para text = "\n"++text++"\n"

blockquote :: Block -> String
blockquote block = para $ break++ascBlock block ++ break
  where
    break = "==================================="

oListItem :: [Block] -> String
oListItem blocks = " # "++(strip $ ascBlocks blocks) ++ "\n"

bListItem :: [Block] -> String
bListItem blocks = " * "++(strip $ ascBlocks blocks) ++ "\n"

strip :: String -> String
strip = t . reverse . t .reverse
  where t = dropWhile isSpace

strong :: String -> String
strong text = "*"++(strip text)++"*"

definition (term,def) = ascInlines term ++ " :: " ++ (unlines $ map ascBlocks def) ++ "\n"

ascMeta :: Meta -> String
ascMeta (Meta title authors date) = 
  (header 1 $ ascInlines title) ++ (unwords $ map ascInlines authors) ++ "\n" ++ (ascInlines date)

ascBlocks :: [Block] -> String
ascBlocks = concatMap ascBlock

ascInlines :: [Inline] -> String
ascInlines = concatMap ascInline

ascBlock :: Block -> String
ascBlock (Plain lst) = ascInlines lst
ascBlock (Para lst) = para $ ascInlines lst
ascBlock (RawHtml s) = s
ascBlock (BlockQuote lst) = concatMap blockquote lst
ascBlock (OrderedList _ items) = para $ concatMap oListItem items
ascBlock (BulletList items) = para $ concatMap bListItem items
ascBlock (DefinitionList lst) = para $ concatMap definition lst
ascBlock (Header n lst) = header n $ ascInlines lst
ascBlock (Table caption _ _ headers rows) =
    ('.': ascInlines caption) ++ "\n"
    ++ "|==============================================================\n"
    ++ "|" ++ (intercalate "|" $ map ascBlocks headers) ++ "|\n"
    ++ unlines ["|" ++ (intercalate "|" $ map ascBlocks cells) ++ "|\n" | cells <- rows]
    ++ "|==============================================================\n"
ascBlock HorizontalRule = "----\n"
ascBlock Null = ""
ascBlock x = error $ "Unsupported block: " ++ show x

quoteWith :: String -> [Inline] -> String
quoteWith s lst = s ++ ascInlines lst ++ s

ascInline :: Inline -> String
ascInline (Str s) = s
ascInline (Emph lst) = quoteWith "'" lst
ascInline (Strong lst) = strong $ ascInlines lst
ascInline (Strikeout lst) = quoteWith "-" lst
ascInline (Superscript lst) = quoteWith "^" lst
ascInline (Subscript lst) = quoteWith "~" lst
ascInline (SmallCaps lst) = ascInlines lst
ascInline (Quoted t lst) = quoteWith qt lst
  where qt | SingleQuote <- t = "'"
           | DoubleQuote <- t = "\""
ascInline (Cite tgt lst) = undefined
ascInline (Code s) = unlines $ map ("    "++) $ lines s
ascInline Space = " "
ascInline EmDash = "---"
ascInline EnDash = "--"
ascInline Apostrophe = "'"
ascInline Ellipses = "..."
ascInline LineBreak = "\\\\"
ascInline (Math _ s) = s
ascInline (TeX s) = s
ascInline (HtmlInline s) = s
ascInline (Link title (url,_)) = "link:" ++ url ++ "[" ++ ascInlines title++"]"
ascInline (Image _ (url,_)) = "image:"++url++"[]"
ascInline (Note blocks) = ascBlocks blocks

