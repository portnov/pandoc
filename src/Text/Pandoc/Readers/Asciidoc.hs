{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Pandoc.Readers.Asciidoc
  (readAsciidoc)
  where

import Prelude hiding (getContents,print, putStrLn)
import System.FilePath (dropExtension)
import Control.Arrow
import Control.Monad (when,liftM)
import Data.Char
import Data.Maybe
import Data.List
import Data.Function (on)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec
import qualified Text.Pandoc.Definition as P
import Text.Pandoc.Shared (ParserState)

-- | First name, second name, surname, email
data Author = Author String String String String

nobody ::  Author
nobody = Author "" "" "" ""

instance Show Author where
  show (Author fname sname sur em) = fname ++ " " ++ sname ++ " " ++ sur ++ " <"++em++">"

-- | version number, date, description
data Version = Version String String String
    deriving (Show)
data Preamble = Preamble (Maybe Author) (Maybe Version)
    deriving (Show)

type Attributes = [(String,String)]

data Attributed a = Attributed {
                      anchor :: Maybe String,
                      attributes :: Attributes,
                      title :: Maybe String,
                      content :: a }

woAttributes ::  a -> Attributed a
woAttributes x = Attributed Nothing [] Nothing x

instance (Show a) => Show (Attributed a) where
  show (Attributed anc attr tit cont) = 
        (maybe "" (\s -> "[["++s++"]]\n") anc)
     ++ showAtts attr
     ++ (maybe "" (\s -> "."++s++"\n") tit)
     ++ show cont
   where
    showAtts [] = ""
    showAtts lst = show lst

data Inline = Macro String String [String] -- ^ macro name, first arg, other args
            | Quoted QuoteType String
            | InternalLink String String   -- ^ link target, link text
            | Text String

data Cell = Cell Int Int Char Char Char String [Block]
    deriving (Show)

instance Show Inline where
  show (Macro n a as) = n ++ ":" ++ a ++ show as
  show (Quoted t s) = "<"++show t++" "++s++">"
  show (InternalLink h t) = "<<"++h++","++t++">>"
  show (Text s) = s

type TextLine = [Inline]

data ListItem = ListItem { lstLevel :: Int, lstLine :: TextLine }
    deriving (Show)

data ListNode = ListNode TextLine [ListNode] -- ^ Item itself, item's children
    deriving (Show)

lstChildren :: ListNode -> [ListNode]
lstChildren (ListNode _ list) = list

data QuoteType = Strong | Emphasis | Unquoted | Strikeout | Mono | Superscript
    deriving (Show)

data AdmType = NOTE | TIP | IMPORTANT | CAUTION | WARNING | TODO
    deriving (Show,Read)

data Block  = Header Int String
            | Para TextLine
            | AdmPara AdmType TextLine
            | Code [String]
            | BulletedList [ListNode]
            | NumberedList [TextLine]
            | DefList [(TextLine,TextLine)]
            | Table [Cell] [[Cell]]
            | Delimited Char [TextLine]

instance Show Block where
  show (Header n h) = "\nH"++show n ++". "++h++"\n"
  show (Para lst) = "P:" ++ showL lst
  show (AdmPara t lst) = show t ++ ": " ++ showL lst
  show (Code lst) = "\n"++intercalate "\n" lst ++ "\n"
  show (BulletedList lst) = "\n{"++ concatMap (\l -> " * "++show l++"\n") lst ++ "}\n"
  show (NumberedList lst) = "\n"++ concatMap (\l -> " # "++show l++"\n") lst ++ "\n"
  show (DefList lst) = "\n"++ concatMap (\(t,d) -> show t++" :: "++show d++"\n") lst ++ "\n"
  show (Table _ lst) = show lst
  show (Delimited c lst) = del ++ show lst ++ "\n" ++ del
      where del = replicate 5 c ++ "\n"

showL ::  (Show a) => [a] -> String
showL lst = intercalate " " (map show lst)++"\n"

maybeP ::  GenParser tok st a -> GenParser tok st (Maybe a)
maybeP p = 
    let tmp = p >>= (return . Just)
    in  option Nothing (try tmp)

toEnd ::  (Show tok) => GenParser tok st b -> GenParser tok st b
toEnd p = do
    r <- p
    eof
    return r

untitled ::  Attributed Block
untitled = woAttributes $ Header 1 ""

whitespace ::  Parser String
whitespace = (many1 $ oneOf " \t") <?> "whitespace"

indent :: Parser String
indent = (many $ oneOf " \t") <?> "indent"

(!) ::  (Ord k) => M.Map k a -> k -> a
(!) = (M.!)

newlines ::  String
newlines = ['\n', chr 13]

headerChars ::  M.Map Int Char
headerChars = M.fromList [(1,'='), (2,'-'), (3,'~'), (4,'^')]

quotedBy ::  Char -> QuoteType -> Parser Inline
quotedBy c t = try $ do
    char c
    text <- many1 $ noneOf [c,'\n']
    char c
    many $ oneOf " \t"
    return $ Quoted t text

pQuoted ::  Parser Inline
pQuoted = (quotedBy '*' Strong)
      <|> (quotedBy '\'' Emphasis)
      <|> (quotedBy '_' Emphasis)
      <|> (quotedBy '~' Strikeout)
      <|> (quotedBy '^' Superscript)
      <|> (quotedBy '+' Mono)
      <|> (quotedBy '#' Unquoted)

pNewLine' ::  Parser String
pNewLine' = ((try $ string "\n\r") <|> (try $ string "\r\n") <|> (string "\n")) <?> "newline"

pNewLine ::  Parser String
pNewLine = pNewLine' >> (many $ oneOf " \t")

quoted ::  Parser String
quoted = between (char '"') (char '"') (many $ noneOf "\"")

pOneAttrib ::  Parser (String, String)
pOneAttrib = do
  name <- many1 $ noneOf "="
  char '='
  value <- quoted
  return (name, value)

pManyAttrs ::  Parser [(String, String)]
pManyAttrs = pOneAttrib `sepBy` (char ',')

pAttributes ::  Parser [(String, String)]
pAttributes = do
  char '['
  l <- pManyAttrs
  char ']'
  optional pNewLine
  return l

pAnchor ::  Parser (String)
pAnchor = do
  string "[["
  text <- many1 $ noneOf "]"
  string "]]"
  pNewLine
  return text

pPlainText ::  Parser Inline
pPlainText = do
    text <- many1 $ noneOf " \t*#'~\n\r"
    optional whitespace
    return $ Text text

pCellText :: Parser Inline
pCellText = do
    text <- many1 $ noneOf "| \t*#'~\n\r"
    optional whitespace
    return $ Text text

pMacro ::  Parser Inline
pMacro = do
  name <- many1 alphaNum
  char ':'
  arg1 <- many1 $ noneOf " \t\n\r["
  char '['
  args <- many $ noneOf "\n\r]"
  char ']'
  optional whitespace
  return $ Macro name arg1 [args]

pInternalLink ::  Parser Inline
pInternalLink = do
  string "<<"
  href <- many1 $ noneOf ",\n"
  char ','
  linkTitle <- many1 $ noneOf ">\n"
  string ">>"
  optional whitespace
  return $ InternalLink href linkTitle

inline ::  Parser Inline
inline = do
--     many $ oneOf " \t"
    (try pQuoted)
    <|> (try pInternalLink)
    <|> (try pMacro)
    <|> pPlainText

cellInline :: Parser Inline
cellInline = try pQuoted
         <|> try pInternalLink
         <|> try pMacro
         <|> pCellText

pNormalLine ::  Parser [Inline]
pNormalLine = do 
  text <- many1 inline
  pNewLine
  return text

pCommentLine ::  Parser [a]
pCommentLine = do
  string "//" <?> "comment start marker"
  anyChar `manyTill` pNewLine
  return []

pAttributeLine :: Parser [a]
pAttributeLine = do
  char ':' <?> "attribute start marker"
  many1 alphaNum
  char ':' <?> "attribute separator"
  many $ oneOf " \n"
  anyChar `manyTill` pNewLine
  return []

pAttributesPara :: Parser [a]
pAttributesPara = do
  many1 pAttributeLine
  many pNewLine
  return []

pLine ::  Parser [Inline]
pLine = pAttributeLine <|> pCommentLine <|> pNormalLine 

concatP ::  (Monad m) => m [[a]] -> m [a]
concatP = liftM concat

pCodeLine ::  Int -> Parser String
pCodeLine n = do
  (string $ replicate n ' ') <?> (show n ++ " spaces")
  line <- anyChar `manyTill` pNewLine'
  return line

pCode ::  Parser Block
pCode = do
  lst <- choice $ map (many1 . pCodeLine) [2..8]
  many1 pNewLine'
  return $ Code lst

pParagraph ::  Parser Block
pParagraph = do
    t <- concatP $ many1 pLine
    many1 pNewLine'
    return $ Para t

pCellParagraph :: Parser Block
pCellParagraph = do
    t <- concatP $ many1 pNormalLine 
    return $ Para t

pAdmParagraph ::  Parser Block
pAdmParagraph = do
    t <- choice $ map (try . string . (++":")) ["NOTE","TIP","IMPORTANT","CAUTION","WARNING","TODO"]
    whitespace
    text <- concatP $ many1 pLine
    many1 pNewLine
    return $ AdmPara (read $ init t) text

pAnyParagraph ::  Parser Block
pAnyParagraph = (try pCode) <|> (try pAdmParagraph) <|> pParagraph

pCellBlock :: Parser Block
pCellBlock = choice $ map try $ [
    pCellParagraph,
    pCode,
    pAdmParagraph,
    pBulletedList,
    pNumberedList,
    pDefList ]

pBulletedListItem ::  Char -> Parser ListItem
pBulletedListItem c = do
  indent
  cc <- many1 $ char c
  let n = length cc
  whitespace
  item <- inline `manyTill` pNewLine
  return $ ListItem n item 

pNumberedListItem ::  Parser [Inline]
pNumberedListItem = do
  indent
  many1 digit
  char '.'
  whitespace
  lst <- inline `manyTill` pNewLine
  return lst

pDefListItem ::  Parser ([Inline], [Inline])
pDefListItem = do
  term <- inline `manyTill` (string "::")
  whitespace
  optional (pNewLine >> (many $ oneOf " \t"))
  def <- pLine
  return (term, def)

unfoldTree ::  (a -> (TextLine, [a])) -> a -> ListNode
unfoldTree g = uncurry ListNode . second (map (unfoldTree g)) . g

splitBy ::  (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p s  = filter (not . null) $ unfoldr (\c -> if null c
                                                    then Nothing
                                                    else Just $ first (head c :) $ break p $ tail c) s

sameLevel ::  ListItem -> ListItem -> Bool
sameLevel = (==) `on` lstLevel

listTransform ::  [ListItem] -> [ListNode]
listTransform = lstChildren . unfoldTree (\(s:ss) -> (lstLine s, splitBy (sameLevel (head ss)) ss)) . (ListItem 0 []:)

pBulletedList ::  Parser Block
pBulletedList = do
    lst <- choice $ map many1 $ [pBulletedListItem '*', pBulletedListItem '-', pBulletedListItem '.']
    many pNewLine
    return $ BulletedList $ listTransform lst

pNumberedList ::  Parser Block
pNumberedList = do
    lst <- many1 pNumberedListItem
    many1 pNewLine
    return $ NumberedList lst

pDefList ::  Parser Block
pDefList = do
    lst <- many1 pDefListItem
    many1 pNewLine
    return $ DefList lst

pDelimitedBlock ::  Char -> Parser Block
pDelimitedBlock c = do
    del <- many1 (char c)
    let n = length del
    let pDel = do
               string $ replicate n c
               pNewLine
    pNewLine
    lst <- pLine `manyTill` (try pDel)
    many pNewLine
    return $ Delimited c lst

pAnyDelimitedBlock ::  Parser Block
pAnyDelimitedBlock = 
      (pDelimitedBlock '=') 
  <|> (pDelimitedBlock '-')
  <|> (pDelimitedBlock '~')
  <|> (pDelimitedBlock '+')
  <|> (pDelimitedBlock '/')

pBlockTitle ::  Parser [Char]
pBlockTitle = do
  char '.' <?> "block title marker"
  blockTitle <- anyChar `manyTill` pNewLine
  return blockTitle
  
pAttributed :: (Attributes -> Parser t) -> Parser (Attributed t)
pAttributed p = do
  an <- maybeP pAnchor
  tt <- maybeP pBlockTitle 
  at <- option [] (try pAttributes)
  r <- p at
  return $ Attributed an at tt r

pAnyHeader ::  Parser Block
pAnyHeader = (try $ pHeader 1) <|> (try $ pHeader 2) <|> (try $ pHeader 3) <|> (try $ pHeader 4)

pHeader ::  Int -> Parser Block
pHeader l = (pOneLineHeader l) <|> (pTwoLineHeader l $ headerChars ! l)

pOneLineHeader ::  Int -> Parser Block
pOneLineHeader l = do
    (string $ (replicate l '=') ++ " ") <?> "one-line header marker"
    text <- many1 (noneOf newlines)
    many1 pNewLine
    return $ Header l text

pTwoLineHeader ::  Int -> Char -> Parser Block
pTwoLineHeader l c = do
    text <- many1 (noneOf newlines)
    pNewLine
    let n = length text
    (string $ replicate n c) <?> "rule after header"
    when (l>1) (pNewLine >> return ())
    many pNewLine
    return $ Header l text

pTableDelimiter ::  Parser Int
pTableDelimiter = do
  char '|'
  d <- many1 $ char '='
  pNewLine
  return $ length d

pCellSpanSpec :: Parser (Int, Int, Char)
pCellSpanSpec = do
  (cs,rs) <- pCellSpan
  c <- (char '+') <|> (char '*')
  return (cs,rs, c)

pCellAlignChar ::  CharParser st Char
pCellAlignChar = oneOf "<^>"

pCellAlign ::  Parser (Char, Char)
pCellAlign = do
  hor <- pCellAlignChar
  vert <- option '<' $ try $ do
      char '.' <?> "cell align separator"
      pCellAlignChar 
  return (hor,vert)

pCellSpan ::  Parser (Int, Int)
pCellSpan = do
    cs <- many1 digit
    rs <- option 1 $ try $ do
        char '.' <?> "cell span separator"
        ns <- many1 digit
        return $ read ns
    return (read cs,rs)

pCellStyle ::  Parser String
pCellStyle = many1 alphaNum

ignore :: (Monad m) => m a -> m ()
ignore m = m >> (return ())

pCell ::  Parser Cell
pCell = do
    (colspan, rowspan, spantype) <- (option (1,1,'+') $ try pCellSpanSpec) <?> "cell span spec"
    (horalign,vertalign) <- (option ('<','^') $ try pCellAlign) <?> "cell align"
    style <- option "default" $ try pCellStyle
    char '|' <?> "cell separator"
    cellContent <- try pCellMultiline <|> pCellInline
    many whitespace
    return $ Cell colspan rowspan spantype horalign vertalign style cellContent
  where
    pCellInline = do
      whitespace
      c <- many (try cellInline)
      many whitespace
      return [Para c]
    pCellMultiline = do
      pNewLine
      blocks <- ((try pCellBlock) `manyTill` (char '|')) <?> "blocks inside table cell"
--       many pNewLine
      inp <- getInput
      setInput $ "\n|" ++ inp
      return blocks

pTableRow ::  Parser [Cell]
pTableRow = do
  cells <- pCell `manyTill` pNewLine
  return cells

pTable :: Attributes -> Parser Block
pTable attrs = do
  pTableDelimiter 
  rows <- pTableRow `manyTill` (try pTableDelimiter)
  many pNewLine
  case lookup "options" attrs of
    Just "header" -> return $ Table (head rows) (tail rows)
    _             -> return $ Table [] rows

pEmail ::  Parser String
pEmail = do
  char '<'
  user <- many1 (oneOf ".-" <|> alphaNum)
  char '@'
  host <- many1 (oneOf ".-" <|> alphaNum)
  char '>'
  return $ user ++ "@" ++ host

pAuthor ::  Parser Author
pAuthor = do
  fname <- many1 (noneOf " \t")
  whitespace
  sname <- many1 (noneOf " \t")
  whitespace
  srname <- many1 (noneOf " \t")
  whitespace
  mail <- pEmail
  optional whitespace
  return $ Author fname sname srname mail

pVersion ::  Parser Version
pVersion = do
  char 'v'
  num <- many1 (noneOf ",\n")
  char ','
  whitespace
  date <- many1 (noneOf ":")
  char ':'
  whitespace
  descr <- many1 (noneOf newlines)
  return $ Version num date descr

pPreamble ::  Parser Preamble
pPreamble = do
  a <- pAuthor
  pNewLine
  v <- maybeP pVersion
  many1 pNewLine
  return $ Preamble (Just a) v

pDocumentHeader :: Parser (Attributed Block, Maybe Preamble)
pDocumentHeader = do
    optional pAttributesPara
    h <- pAttributed $ (const $ pHeader 1)
    p <- maybeP pPreamble
    return (h,p)

body ::  Parser [Attributed Block]
body = many1 $ choice $ map (try . pAttributed) [
    const pAnyHeader,
    pTable,
    const pNumberedList,
    const pBulletedList,
    const pDefList,
    const pAnyDelimitedBlock,
    const pAnyParagraph ]

asciidoc :: Parser (Attributed Block, Maybe Preamble, [Attributed Block])
asciidoc = do
    pp <- maybeP pDocumentHeader
    let (h,p) = fromMaybe (untitled, Nothing) pp
    lst <- body
    return (h,p,lst)

pandoc :: (Attributed Block, Maybe Preamble, [Attributed Block]) -> P.Pandoc
pandoc (h, preamble,lst) = P.Pandoc meta $ concatMap (toPandocA anchors) lst
  where
    anchors = catMaybes $ map anchor lst
    meta = P.Meta [P.Str doctitle] [[P.Str $ show author]] [P.Str date]
    Header _ doctitle = content h
    Version _ date _ = fromMaybe (Version "0.0" "" "") version
    (author, version) = case preamble of
                          Just (Preamble (Just a) mv) -> (a, mv)
                          Just (Preamble Nothing mv) -> (nobody, mv)
                          Nothing -> (nobody, Nothing)

nullAttr ::  (String, [a], [a1])
nullAttr = ("",[],[])

defListAttr ::  (Int, P.ListNumberStyle, P.ListNumberDelim)
defListAttr = (1, P.DefaultStyle, P.DefaultDelim)

pandocListItem :: [String] -> ListNode -> [P.Block]
pandocListItem a (ListNode item []) = [P.Plain $ pandocInlineMap a item]
pandocListItem a (ListNode item children) = [P.Plain $ pandocInlineMap a item, P.BulletList $ map (pandocListItem a) children]

toPandocA :: [String] -> Attributed Block -> [P.Block]
toPandocA anchors blk =
  let mbTitle = case title blk of
                  Just tt -> [P.Para [P.Strong [P.Str tt]]]
                  Nothing -> []
      mbAnchor = case anchor blk of
                  Just a -> [P.Para [P.Anchor a []]]
                  Nothing -> []
  in mbAnchor ++ mbTitle ++ [toPandoc anchors (content blk)]

toPandoc :: [String] -> Block -> P.Block
toPandoc _ (Header n str) = P.Header n [P.Str str]
toPandoc a (Para lst) = P.Para $ pandocInlineMap a lst
toPandoc a (AdmPara t lst) = P.BlockQuote [P.Para $ [P.Strong [P.Str $ show t], P.Str ":", P.Space] ++ pandocInlineMap a lst]
toPandoc _ (Code lst) = P.CodeBlock nullAttr $ intercalate "\n" lst
toPandoc a (BulletedList lst) = P.BulletList $ map (pandocListItem a) lst
toPandoc a (NumberedList lst) = P.OrderedList defListAttr [[P.Plain $ pandocInlineMap a item] | item <- lst]
toPandoc a (DefList lst) = P.DefinitionList [(pandocInlineMap a t, [[P.Plain $ pandocInlineMap a d]]) | (t,d) <- lst]
toPandoc a (Delimited _ lst) = P.BlockQuote [P.Plain $ pandocInlineMap a line | line <- lst]
toPandoc a (Table hdr lst) = P.Table [] (map alignment $ head lst) [] (map pandocCell hdr) [[pandocCell cell | cell <- row] | row <- lst]
  where
    alignment (Cell _ _ _ h _ _ _) = alignment' h
    alignment' '<' = P.AlignLeft
    alignment' '>' = P.AlignRight
    alignment' '^' = P.AlignCenter
    alignment' _ = P.AlignLeft
    pandocCell (Cell _ _ _ _ _ _ cells) = cell' cells
    cell' blocks = map (toPandoc a) blocks

pandocInlineMap :: [String] -> [Inline] -> [P.Inline]
pandocInlineMap ans = concatMap (pandocInline ans)

getImg :: String -> String
getImg "" = ""
getImg (':':s) = '/':s
getImg s = '/': s

pandocInline :: [String] -> Inline -> [P.Inline]
pandocInline _ (Text s) = [P.Space, P.Str s]
pandocInline _ (Macro name arg args) = 
    case name of
      "link" -> [P.Space, P.Link (map P.Str args) (getLinkFile arg,"")]
      "image" -> [P.Image (map P.Str args) (getImg arg,"")]
      "include" -> [P.Space, P.Link [P.Str "Include:",P.Space, P.Str $ tail arg] (dropExtension $ tail arg,"")]
      _ -> []
pandocInline _ (Quoted qt s) = 
    case qt of
      Strong -> [P.Space,P.Strong [P.Str s]]
      Strikeout -> [P.Space,P.Strikeout [P.Str s]]
      Emphasis -> [P.Space,P.Emph [P.Str s]]
      Unquoted -> [P.Space,P.Str s]
      Mono -> [P.Space, P.Code s]
      Superscript -> [P.Space, P.Superscript [P.Str s]]
pandocInline anchors (InternalLink h t) =
    if h `elem` anchors
      then [P.Space, P.InternalLink [P.Str t] h]
      else [P.Space, P.Str t]

getLinkFile :: String -> String
getLinkFile href = dropExtension f
  where
    (f,_) = span (/='#') href

getLink :: String -> String
getLink href = (dropExtension f) ++ a
  where
    (f,a) = span (/='#') href

-- removeComments :: String -> String
-- removeComments = unlines . map removeComment . lines
--   where
--     removeComment ('/':'/':_) = ""
--     removeComment x           = x

removeComments :: String -> String
removeComments = unlines . filter isNotComment . lines
  where
    isNotComment ('/':'/':_) = False
    isNotComment _           = True

readAsciidoc :: ParserState -> String -> P.Pandoc
readAsciidoc _ text = case parse asciidoc "<input>" text' of
    Left e -> error $ show e
    Right x -> pandoc x
  where
    text' = removeComments (text ++ "\n\n")

