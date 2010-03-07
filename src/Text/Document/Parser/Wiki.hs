{-# LANGUAGE FlexibleContexts #-}
module Text.Document.Parser.Wiki where

import Control.Applicative hiding ((<|>))
import Data.List (intercalate)
import Data.Maybe (isNothing, catMaybes)
import Prelude hiding (lines)
import Text.ParserCombinators.Parsec hiding (many, optional)

import Text.Document.Core.Type
import Text.Document.Plugin
import Text.Document.PluginRegister
import Text.Document.Parser.WikiHelper
import Network.Protocol.Uri
import Network.Protocol.Uri.Parser (pAbsoluteUri)
import Misc.Commons

-------[ document parsers ]----------------------------------------------------

pDocTitle    = string "###"  *> (text <$> pNonEmptyLine) <* many pEmptyLineEol
pDocSubtitle = string "####" *> (text <$> pNonEmptyLine) <* many pEmptyLineEol
pAbstract    = string "**!"  *> many pEmptyLineEol *> (snd <$> pParagraph 0)
pAnnotate    = char '@'      *> ((,) <$> pString <*> (snd <$> pInline 0))

pDocument = document 
  <$> pMaybe pDocTitle
  <*> pMaybe pDocSubtitle
  <*> many   (try pAnnotate)
  <*> pMaybe (try pAbstract)
  <*> (maybe [] snd <$> pMaybe (pSections 0 False))

parseDocumentFile file = do
  f <- readFile file
  return $ (pDocument @! f)

fromWiki :: String -> Either ParseError Document
fromWiki t = parse pDocument "" (t ++ "\n")

-------[ inline parsers ]------------------------------------------------------

-- TODO: refactor + generalize

-- Several links types.
pWebLink = (\a -> link (show a) External (text $ show a)) <$> pAbsoluteUri

pReference = (\a f -> link a Reference (parseInline (f a)))
        <$> (string "[#" *> many (noneOf "|]")) <*> href <* char ']'
  where href = option id (const <$> (char '|' *> many (noneOf "]")))

pInternalLink = (\a f -> link (f a) Internal (text a)) <$> (char '[' *> many (noneOf "|]")) <*> href <* char ']'
  where href = option id (const <$> (char '|' *> many (noneOf "]")))

pImage = (\a f -> image a (f a)) <$> (char '{' *> many (noneOf "|}")) <*> href <* char '}'
  where href = option id (const <$> (char '|' *> many (noneOf "}")))

-----

pAbbreviations =
    choice
  $ map (\(a, b) -> text b <$ string a) [
    (">>",  "»")
  , ("<<",  "«")
  , ("...", "…")
  , ("--",  "―")
  ]

pCustoms =
      try pWebLink
  <|> try pReference
  <|> try pInternalLink
  <|> try pImage
  <|> try pAbbreviations

inlineTbl = [
    ("*",   "*",  strong)
  , ("__",  "__", footnote)
  , ("_",   "_",  emph)
  , ("==",  "==", quote)
  , ("=",   "=",  fixed)
  , ("\0",  "\0", id)
  ]

-- tricky helper. explain
pFragmentText sym = lines <$> f <$> many (Left <$> pCustoms <|> Right <$> noneOf sym)
  where 
    f []          = []
    f (Left l:xs) = l : f xs
    f xs          = text (map (\(Right x) -> x) a) : f b
      where (a, b) = span right xs

pFragment (s, e, sem) = (sem . lines) <$> (begin *> frags)
  where
    begin  = try (string s)
    end    = try (string e)
    sym    = head s : map (\(_, b, _) -> head b) inlineTbl
    normal = pFragmentText sym
    more   = (\a b c -> (text [a] <++> b) : c) <$> oneOf sym <*> normal <*> frags
    sub    = (:) <$> pLines <*> frags
    frags  = normal <**> option pure (pure <$ end <|> (flip(:)) <$> sub <|> (flip(:)) <$> more)

pLines = choice $ map pFragment inlineTbl

parseInline :: String -> Inline
parseInline xs =
    either (text . show) id
  $ parse pLines "" ('\0' : xs ++ "\0")

pInline :: Int -> GenParser Char st (Int, Inline)
pInline n = fmap2 id (parseInline . trimText) <$> pText n

-------[ structural parsers ]--------------------------------------------------

pSections :: Int -> Bool -> GenParser Char () (Int, SectionContents)
pSections n forceTitle = do

  -- Based on the `forceTitle' flag we parse or try to parse a title.
  (n', title) <-
    (if forceTitle
      then id
      else option (n, Nothing))
    (try $ pTitle n)

  -- Try to parse another section on level deeper OR another section on the
  -- same level, this section MUST have a title of its own (otherwise it is not
  -- a new section) OR a new block level element.
  items <- some $ choice [
      (Left                 . snd) <$> pSections (n' + 1) (isNothing title)
    , (Right .                snd) <$> pSections n' True
    , (Left  . (:[]). Right . snd) <$> pBlock    n'
    ]

  let -- Separate all sub-sections from block elements that are ours
      (sub, next) = partitionEither items

      -- Make this section.
      this = section title (concat sub)

  return (n', (Left this) : concat next)

pTitle n = (,)
  <$> (pIndent' n <* string "***")
  <*> (Just . snd <$> pInline 0)

-------[ block parsers ]-------------------------------------------------------

-- Try to parse a single block level element.

-- TODO: These try's should probably be on item indicator level.

pBlock n =
      try (pPlugin        n)
  <|> try (pInclude       n)
  <|> try (pAnchor        n)
  <|> try (pCaption       n)
  <|> try (pMapping       n)
  <|> try (pUnorderedList n)
  <|> try (pOrderedList   n)
  <|>     (pParagraph     n)

{-
Collect all plugins from the global plugin register that claim can parser
Wiki fragments.
-}

pluginParsers = catMaybes $ map (\p -> transformer p Wiki) pluginRegister

pPlugin n = (,) n
  <$> plugin <$> choice (map (\a -> try $ a $ Just (ContextWiki n)) pluginParsers)
  <*  many pEmptyLineEol

pAnchor n = do
  n' <- pIndent' n
  string "#anchor "
  l <- pNonEmptyLine
  (_, b) <- pBlock n'
  return $ (n', anchor_ l b)

pCaption n = do
  n' <- pIndent' n
  string "#caption "
  (_, i) <- pInline 0
  (_, b) <- pBlock n'
  return $ (n', caption i b)

pInclude n = do
  n' <- pIndent' n
  i <- pInclude'
  many pEmptyLineEol
  return $ (n', i)

pInclude' = (\a f -> include (f a))
       <$> (string "[*" *> many (noneOf "|]")) <*> href <* char ']'
  where href = option id (const <$> (char '|' *> many (noneOf "]")))

pParagraph n = fmap2 id paragraph <$> pInline n

-- List parsers.

pMapping       = pList mapping (pMapItem  "--" "->")
pUnorderedList = pList list    (pListItem "-")
pOrderedList   = pList enum    (pListItem "+")

pList sem item n = do
  (n', x) <- item n
  xs <- map snd <$> many (try $ item n')
  return (n', sem (x:xs))

pListItem c n = do
  n' <- pIndent' n
  i <- string c
     *> ((\a b -> blocks (a : b))
    <$> (snd <$> pBlock 0)
    <*> (map snd <$> many (pBlock (n'+1))))
  return (n', i)

pMapItem c0 c1 n = do
  (n', a) <- pListItem c0 n
  (_,  b) <- pListItem c1 n
  return (n', (a, b))

