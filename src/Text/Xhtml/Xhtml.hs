module Text.Xhtml.Xhtml where

import Prelude hiding (head, div, span)

import Text.Xml.Xml hiding (text)
import qualified Text.Xml.Xml

-------[ XHTML shortcuts ]-----------------------------------------------------

-- Document structure elements.

document t b = html $ nodeset [head (title t), body b]

html     = doc . element "html"
head     = element "head"
title    = element "title" . text
body     = element "body"

-- Title elements.

h1       = element "h1"
h2       = element "h2"
h3       = element "h3"
h4       = element "h4"
h5       = element "h5"
h6       = element "h5"
header i = element ("h" ++ show i)

-- Block level elements.

div      = element "div"
pre      = element "pre"
par      = element "p"
input    = element "input"
ul       = element "ul" . nodeset . map (element "li")
ol       = element "ol" . nodeset . map (element "li")
dlterm   = element "dt"
dldef    = element "df"
dl       = element "dl" . nodeset . concat . map (\(k, v) -> [element "dt" k, element "dd" v])

-- Inline elements.

text     = Text.Xml.Xml.text
span     = element "span"
img s a  = attribute "src"  s $ attribute "alt" a $ element "img" $ text ""
anchor h = attribute "href" h . element "a"
link h   = anchor h . text
emph     = element "em"
strong   = element "strong"
tt       = element "tt"
dfn      = element "dfn"
code     = element "code"
cite     = element "cite"
hr       = element "hr" $ nodeset []

-- Shortcuts for custom elements using classes.

withClass c e = attribute "class" c . e

classDiv  = flip withClass div 
classSpan = flip withClass span

withName n e = attribute "name" n . e

nameAnchor = flip withName (element "a")
nameDiv    = flip withName div
nameSpan   = flip withName span

