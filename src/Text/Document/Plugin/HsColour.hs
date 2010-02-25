module Text.Document.Plugin.HsColour (hscolourPlugin) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Language.Haskell.HsColour.Colourise       as C
import qualified Language.Haskell.HsColour.ColourHighlight as H
import qualified Language.Haskell.HsColour.CSS             as HscCSS
import qualified Language.Haskell.HsColour.LaTeX           as HscTeX

import Text.Document.Parser.WikiHelper
import Text.Document.Core.Type
import Text.Document.Plugin
import qualified Text.Xhtml.Xhtml as H
import qualified Text.Xml.Xml     as X

-------- hscolour plugin ------------------------------------------------------

hscolourPlugin :: Plugin Document Syn_Document
hscolourPlugin = makePlugin "hscolour" 
  [(Wiki,  pWiki)]
  [(XHTML, ppXHTML), (LaTeX, ppLaTeX), (ADT, ppADT)]
  []

-------- parsers --------------------------------------------------------------

pWiki (Just (ContextWiki n)) = do
   (m, i) <- pIndent n
   (string "@@" <* many (oneOf ws) <* string "haskell")
     *> many pEmptyLineEol
     *> (trimCode
    <$> pLevel (m + 1))

-------- pretty printers ------------------------------------------------------

ppXHTML s doc = OutputXML $ X.snippet $ HscCSS.hscolour False s

ppLaTeX s doc = OutputPlain
  $ "\\begin{verbatim}\n" ++ s ++ "\n\\end{verbatim}\n"

{-
ppLaTeX s = OutputPlain
  $ HscTeX.hscolour texColourPrefs True s

texColourPrefs = C.ColourPrefs {
    C.keyword          = [H.Normal]
  , C.keyglyph         = [H.Normal]
  , C.layout           = [H.Normal]
  , C.comment          = [H.Normal]
  , C.conid            = [H.Normal]
  , C.varid            = [H.Normal]
  , C.conop            = [H.Normal]
  , C.varop            = [H.Normal]
  , C.string           = [H.Normal]
  , C.char             = [H.Normal]
  , C.number           = [H.Normal]
  , C.cpp              = [H.Normal]
  , C.selection        = [H.Normal]
  , C.variantselection = [H.Normal]
  , C.definition       = [H.Normal]
  }
-}

ppADT s doc = OutputPlain $ show doc

