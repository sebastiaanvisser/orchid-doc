module Text.Document.Plugin.TOC (tocPlugin) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import Text.Document.Core.Type
import Text.Document.Plugin
import Text.Document.Parser.WikiHelper
import qualified Text.Xhtml.Xhtml as H
import qualified Text.Xml.Xml     as X

-------- table of contents plugin ---------------------------------------------

tocPlugin :: Plugin Document Syn_Document
tocPlugin = makePlugin "toc"
  [(Wiki,  pTOC)]
  [(XHTML, ppXHTML), (LaTeX, ppCodeLaTeX), (ADT, ppADT)]
  []

-------- parser(s) ------------------------------------------------------------

pTOC (Just (ContextWiki n)) = do
  (m, i) <- pIndent n
  (string "@@" <* many (oneOf ws) <* string "toc") *> many pEmptyLineEol

-------- pretty printer(s) ----------------------------------------------------

ppXHTML _ doc = OutputXML
  $ H.classDiv "toc"
  $ X.nodeset
  $ (H.h2 $ H.text "Contents")
  : documentToXhtmlTOC doc

ppCodeLaTeX s doc = OutputPlain "\\pagebreak\n\\tableofcontents\n\\pagebreak\n"

ppADT _ doc = OutputPlain "ERROR"

