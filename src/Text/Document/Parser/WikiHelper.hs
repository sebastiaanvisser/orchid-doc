module Text.Document.Parser.WikiHelper where

import Control.Applicative
import Data.List (intercalate)
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (many, (<|>))

import Network.Protocol.Uri
import Misc.Commons

-------[ helpers ]-------------------------------------------------------------

{-
Helper function that trims all empty lines from the top and bottom of a text
fragment and removes trailing whitespace and the biggest square block of
heading whitespace.
-}

trimCode :: String -> String
trimCode txt = 
  let
    mi = minimum $ map (length . takeWhile ews) ne
    ne = filter (not . em) ls
    em = all ews
    ews = (`elem` "\t ")
    ls = lines txt
    tl = withReverse (dropWhile ews)
  in intercalate "\n"
   $ trimWith em
   $ map (drop mi)
   $ map tl ls

trimText :: String -> String
trimText txt = 
  let ews = (`elem` "\t ")
  in intercalate "\n"
   $ trimWith (all ews)
   $ map trim
   $ lines txt

-------[ parsers ]-------------------------------------------------------------

eols, ws, wss :: String
eols = "\r\n"
ws   = "\t "
wss  = ws ++ eols

-- Parse end of line, \r\n or \n.

pEol :: GenParser Char st String
pEol = snoc <$> option "" (string "\r") <*> char '\n'

-- Parse a chunk of whitespace, no line endings.

pSpace :: GenParser Char st String
pSpace = many $ oneOf ws

-- Parse an empty line including the line ending.

pEmptyLineEol :: GenParser Char st String
pEmptyLineEol = try ((++) <$> many (oneOf ws) <*> pEol)

-- Try to parse at least level n indentation.

pIndent :: Int -> GenParser Char st (Int, String)
pIndent n = do
  h <- try (many pEmptyLineEol)
  i <- try (pMin n (oneOf ws))
  return (length i, concat h ++ i) -- (\a -> (length a, a)) <$> 


pIndent' :: Int -> GenParser Char st Int
pIndent' n = fst <$> pIndent n

-- Parse a non empty line excluding the EOL character. 

pNonEmptyLine :: GenParser Char st String
pNonEmptyLine = (++) <$> many (oneOf ws) <*> ((:) <$> noneOf wss <*> many (noneOf eols))

-- Parse a non empty line including the line ending.

pNonEmptyLineEol :: GenParser Char st String
pNonEmptyLineEol = (++) <$> pNonEmptyLine <*> (pEol <|> ("" <$ eof))

-- A string identifier not containing any whitespace.

pString :: GenParser Char st String
pString = many $ noneOf wss

-- Parse a block of text terminated by an empty line.
-- todo: include ident space and reuse this function in pLevel.

pText :: Int -> GenParser Char st (Int, String)
pText n = do
  h <- many pEmptyLineEol
  (m, i) <- pIndent n
  b <- many1 (try pNonEmptyLineEol)
  f <- many pEmptyLineEol
  return (m, concat (h ++ (i:b) ++ f))

-- Parse a block of text terminated by an empty line.

pLevel :: Int -> GenParser Char st String
pLevel n = concat . map snd <$> many (pText n)

