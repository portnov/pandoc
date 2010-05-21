{-# LANGUAGE PatternGuards #-}
module Text.Pandoc.Writers.Confluence (writeConfluence) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Data.List (intercalate)
import Data.Char (isSpace)

writeConfluence :: WriterOptions -> Pandoc -> String
writeConfluence _ (Pandoc meta blocks) = 
  confMeta meta ++ "\n\n" ++ confBlocks blocks

header :: Int -> String -> String
header n title = "\nh"++(show n)++". "++title++"\n"

para :: String -> String
para text = "\n"++text++"\n"

blockquote :: Block -> String
blockquote block = para $ "bq. "++confBlock block ++ "\n"

oListItem :: [Block] -> String
oListItem blocks = " # "++(strip $ confBlocks blocks) ++ "\n"

bListItem :: [Block] -> String
bListItem blocks = " * "++(strip $ confBlocks blocks) ++ "\n"

strip :: String -> String
strip = t . reverse . t . reverse
  where t = dropWhile isSpace

strong :: String -> String
strong text = "*"++(strip text)++"*"

definition (term,def) = (strong $ confInlines term) ++ "\n" ++ (unlines $ map confBlocks def) ++ "\n"

confMeta :: Meta -> String
confMeta (Meta title authors date) = 
  (header 1 $ confInlines title) ++ (unwords $ map confInlines authors) ++ " at " ++ (confInlines date)

confBlocks :: [Block] -> String
confBlocks = concatMap confBlock

confInlines :: [Inline] -> String
confInlines = concatMap confInline

confBlock :: Block -> String
confBlock (Plain lst) = confInlines lst
confBlock (Para lst) = para $ confInlines lst
confBlock (RawHtml s) = s
confBlock (BlockQuote lst) = concatMap blockquote lst
confBlock (OrderedList _ items) = para $ concatMap oListItem items
confBlock (BulletList items) = para $ concatMap bListItem items
confBlock (DefinitionList lst) = para $ concatMap definition lst
confBlock (Header n lst) = header n $ confInlines lst
confBlock (Table caption _ _ headers rows) =
    (strong $ confInlines caption) ++ "\n"
    ++ "||" ++ (intercalate "||" $ map (strip . confBlocks) headers) ++ "||\n"
    ++ unlines ["|" ++ (intercalate "|" $ map (strip . confBlocks) cells) ++ "|" | cells <- rows]
    ++ "\n"
confBlock HorizontalRule = "----\n"
confBlock (CodeBlock _ code) = unlines ["{noformat}", code, "{noformat}"]
confBlock Null = ""
confBlock x = error $ "Unsupported block: " ++ show x

quoteWith :: String -> [Inline] -> String
quoteWith s lst = s ++ confInlines lst ++ s

confInline :: Inline -> String
confInline (Str s) = s
confInline (Emph lst) = quoteWith "_" lst
confInline (Strong lst) = strong $ confInlines lst
confInline (Strikeout lst) = quoteWith "-" lst
confInline (Superscript lst) = quoteWith "^" lst
confInline (Subscript lst) = quoteWith "~" lst
confInline (SmallCaps lst) = confInlines lst
confInline (Quoted t lst) = quoteWith qt lst
  where qt | SingleQuote <- t = "'"
           | DoubleQuote <- t = "\""
confInline (Cite tgt lst) = undefined
confInline (Code s) = s                -- !?
confInline Space = " "
confInline EmDash = "---"
confInline EnDash = "--"
confInline Apostrophe = "'"
confInline Ellipses = "..."
confInline LineBreak = "\\\\"
confInline (Math _ s) = s
confInline (TeX s) = s
confInline (HtmlInline s) = s
confInline (Link title (url,_)) = "["++confInlines title++"|"++url++"]"
confInline (Image _ (url,_)) = "!"++url++"!"
confInline (Note blocks) = confBlocks blocks


