module Text.Document.Parser.WikiHelper where

import Control.Applicative
import Data.List (intercalate)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (many, (<|>))

import Network.Protocol.Uri
import Misc.Commons

-------[ helpers ]-------------------------------------------------------------

{-
Helper function that trims all empty lines from the top and bottom of a text
fragment and removes trailing whitespace and the biggest square block of
heading whitespace.
-}

trimCode txt = 
  let
    mi = minimum $ map (length . takeWhile ws) ne
    ne = filter (not . em) ls
    em = all ws
    ws = (`elem` "\t ")
    ls = lines txt
    tl = withReverse (dropWhile ws)
  in intercalate "\n"
   $ trimWith em
   $ map (drop mi)
   $ map tl ls

trimText txt = 
  let
    ws = (`elem` "\t ")
    tl = withReverse (dropWhile ws)
  in intercalate "\n"
   $ trimWith (all ws)
   $ map trim
   $ lines txt

-------[ parsers ]-------------------------------------------------------------

eols = "\r\n"
ws   = "\t "
wss  = ws ++ eols

-- Parse end of line, \r\n or \n.
pEol :: Stream s m Char => ParsecT s u m String
pEol = snoc <$> option "" (string "\r") <*> char '\n'

-- Parse a chunk of whitespace, no line endings.
pSpace :: Stream s m Char => ParsecT s u m String
pSpace = many $ oneOf ws

-- Parse an empty line including the line ending.
pEmptyLineEol :: Stream s m Char => ParsecT s u m String
pEmptyLineEol = try ((++) <$> many (oneOf ws) <*> pEol)

-- Try to parse at least level n indentation.
pIndent :: Stream s m Char => Int -> ParsecT s u m (Int, String)
pIndent n = do
  h <- try (many pEmptyLineEol)
  i <- try (pMin n (oneOf ws))
  return (length i, concat h ++ i) -- (\a -> (length a, a)) <$> 

pIndent' :: Stream s m Char => Int -> ParsecT s u m Int
pIndent' n = fst <$> pIndent n

-- Parse a non empty line excluding the EOL character. 
pNonEmptyLine :: Stream s m Char => ParsecT s u m String
pNonEmptyLine = (++) <$> many (oneOf ws) <*> ((:) <$> noneOf wss <*> many (noneOf eols))

-- Parse a non empty line including the line ending.
pNonEmptyLineEol :: Stream s m Char => ParsecT s u m String
pNonEmptyLineEol = (++) <$> pNonEmptyLine <*> (pEol <|> ("" <$ eof))

-- A string identifier not containing any whitespace.
pString :: Stream s m Char => ParsecT s u m String
pString = many $ noneOf wss

-- Parse a block of text terminated by an empty line.
-- todo: include ident space and reuse this function in pLevel.

pText :: Stream s m Char => Int -> ParsecT s u m (Int, String)
pText n = do
  h <- many pEmptyLineEol
  (m, i) <- pIndent n
  b <- many1 (try pNonEmptyLineEol)
  f <- many pEmptyLineEol
  return (m, concat (h ++ (i:b) ++ f))

-- Parse a block of text terminated by an empty line.
pLevel :: Stream s m Char => Int -> ParsecT s u m String
pLevel n = concat . map snd <$> many (pText n)

