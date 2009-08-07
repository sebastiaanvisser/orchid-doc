module Text.Json.Parser (parseJson) where

import Control.Applicative
import Control.Monad
import Prelude hiding (lex)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

import Misc.Commons
import qualified Format.Json.Json as J

-------[ lexer ]---------------------------------------------------------------

funDef = LanguageDef  {
    commentStart    = "/*" 
  , commentEnd      = "*/" 
  , commentLine     = "//" 
  , nestedComments  = False
  , identStart      = letter <|> char '_'
  , identLetter     = alphaNum <|> oneOf "_"
  , opStart         = pzero
  , opLetter        = pzero
  , reservedOpNames = []
  , reservedNames   = ["true", "false", "null"]
  , caseSensitive   = True 
  } 

lex   = makeTokenParser funDef
res   = reserved lex
ident = identifier lex
op    = reservedOp lex

infixl 0 ?>
(?>) = flip label

-------[ parser definition ]---------------------------------------------------

-- Top level parser.

parseJson = pJson

pNumber = "number" ?>
  J.number <$>
     ((J.R <$> try (float lex))
  <|> (J.Z <$> integer lex))

pBool = "boolean" ?>
      J.bool True  <$ string "true"
  <|> J.bool False <$ string "false"

pNull = "null" ?>
  J.null <$ string "null"

pString = "string literal" ?>
  J.string <$> stringLiteral lex

pPair = "key/value pair" ?>
  J.pair <$> (
      stringLiteral lex
  <|> ident
  <|> (either show show) <$> naturalOrFloat lex
  ) <*> (op ":" *> pJson)

pObject = "object" ?>
  J.object <$> (op "{" *> many (pPair <* (option () $ op ",")) <* op "}")

pArray = "array" ?>
  J.array <$> (op "[" *> many (pJson <* (option () $ op ",")) <* op "]")

pJson = "json literal" ?>
  choice [
    pNumber
  , pBool
  , pNull
  , pString
  , pObject
  , pArray
  ]

